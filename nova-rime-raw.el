;;; nova-rime-raw.el --- Keep raw letters in the buffer while composing -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 和微信输入法、鼠须管一样：输入中 buffer 里放的就是敲下的字母，汉字只出现在下面的
;; 词块里，按空格才整串换成汉字。字母是普通文字，光标可以停在任意字母前后，用普通的
;; Emacs 命令插入、删除；每条命令之后整串重新解码。
;;
;; 解码、冻结、编号和画树都用 nova-rime-tree。这里只管：哪段字母属于输入、按键怎么分派、
;; 在中间改字母后已选的字怎么跟着挪位置、空格怎么上屏。
;;
;; 不用 nova-rime 的半上屏（`nova-rime-edit-mode'）：上屏后的汉字不再带读音，事后不能按读音改。
;; `nova-rime-raw-input' 设为 nil 退回半上屏。

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'nova-rime-tree)

(defvar nova-rime--input)
(defvar nova-rime--pins)
(defvar nova-rime--candidates)
(defvar nova-rime--candidate-records)
(defvar nova-rime--span-choices)
(defvar nova-rime--list-view)
(defvar nova-rime--default-reading)
(defvar nova-rime-english-mode)
(defvar nova-rime-show-candidates)
(defvar nova-rime-reset-hook)
(declare-function nova-rime--parse "nova-rime")
(declare-function nova-rime--candidates "nova-rime")
(declare-function nova-rime--commit "nova-rime")
(declare-function nova-rime--reset "nova-rime")
(declare-function nova-rime--code-char-p "nova-rime")
(declare-function nova-rime--aux-char-p "nova-rime")
(declare-function nova-rime--digit-p "nova-rime")
(declare-function nova-rime--digit-index "nova-rime")
(declare-function nova-rime--punctuation-for "nova-rime")
(declare-function nova-rime--text-read-only-p "nova-rime")
(declare-function nova-rime--format-list "nova-rime")
(declare-function nova-rime-candidate-end "nova-rime")
(declare-function evil-normal-state "evil-states" (&optional arg))

(defcustom nova-rime-raw-input nil
  "非 nil 时输入中 buffer 里是原始字母，空格才换成汉字；nil 用 nova-rime 的半上屏
（`nova-rime-editable-preedit'）或浮动候选。在开启输入法之前设置。"
  :type 'boolean :group 'nova-rime)

(defcustom nova-rime-raw-color-letters t
  "非 nil 时，字母按对应词块的颜色画下划线，光标所在词块的字母高亮。"
  :type 'boolean :group 'nova-rime)

(defface nova-rime-raw-face
  '((t :underline t))
  "正在输入的字母。" :group 'nova-rime)

(defvar-local nova-rime-raw--start nil "输入开头的 marker。")
(defvar-local nova-rime-raw--end nil "输入结尾的 marker，在它前面插入会跟着后移。")
(defvar-local nova-rime-raw--overlays nil "下划线和树。")
(defvar-local nova-rime-raw--codes nil "上一次解码时每个音节的双拼码，用来在中间改字母后挪动已选的字。")
(defvar-local nova-rime-raw--dirty nil "输入里的字母改过，命令结束后要重新解码。")

;;; 输入段

(defun nova-rime-raw--active-p ()
  (and (markerp nova-rime-raw--start) (eq (marker-buffer nova-rime-raw--start) (current-buffer))
       (< nova-rime-raw--start nova-rime-raw--end)))

(defun nova-rime-raw--text ()
  (buffer-substring-no-properties nova-rime-raw--start nova-rime-raw--end))

(defun nova-rime-raw--inside-p ()
  (and (nova-rime-raw--active-p) (<= nova-rime-raw--start (point) nova-rime-raw--end)))

(defun nova-rime-raw--clear-display ()
  (mapc #'delete-overlay nova-rime-raw--overlays)
  (setq nova-rime-raw--overlays nil))

(defun nova-rime-raw--stop ()
  "结束输入：字母原样留在 buffer 里。"
  (nova-rime-raw--clear-display)
  (when (markerp nova-rime-raw--start) (set-marker nova-rime-raw--start nil))
  (when (markerp nova-rime-raw--end) (set-marker nova-rime-raw--end nil))
  (setq nova-rime-raw--start nil nova-rime-raw--end nil nova-rime-raw--codes nil
        nova-rime-raw--dirty nil)
  (remove-hook 'after-change-functions #'nova-rime-raw--after-change t)
  (nova-rime-raw-mode -1)
  ;; `nova-rime--reset' 会再跑 `nova-rime-reset-hook'，这里先置空，避免重入。
  (when nova-rime--input (let ((nova-rime-reset-hook nil)) (nova-rime--reset)))
  (nova-rime-tree--reset))

(defun nova-rime-raw--begin ()
  "在光标处开始一段输入。"
  (setq nova-rime-raw--start (copy-marker (point))
        nova-rime-raw--end (copy-marker (point) t))
  (add-hook 'after-change-functions #'nova-rime-raw--after-change nil t)
  (nova-rime-raw-mode 1))

(defun nova-rime-raw--after-change (start end _old-length)
  (when (and (nova-rime-raw--active-p)
             (<= nova-rime-raw--start end) (<= start nova-rime-raw--end))
    (setq nova-rime-raw--dirty t)))

;;; 在中间改字母后，已选和冻结的字跟着挪

(defun nova-rime-raw--index-map (old new)
  "OLD 和 NEW 两串双拼码，返回把旧音节序号映射到新序号的函数；改动范围里的返回 nil。"
  (let* ((n-old (length old)) (n-new (length new))
         (prefix (cl-loop for a in old for b in new while (equal a b) count t))
         (suffix (cl-loop for a in (reverse old) for b in (reverse new)
                          for k from 0 below (- (min n-old n-new) prefix)
                          while (equal a b) count t)))
    (lambda (i)
      (cond ((< i prefix) i)
            ((>= i (- n-old suffix)) (+ i (- n-new n-old)))))))

(defun nova-rime-raw--remap (codes)
  "输入从 `nova-rime-raw--codes' 变成 CODES：挪动或丢弃已选的字、冻结的词和上一次的词块。"
  (when (and nova-rime-raw--codes (not (equal codes nova-rime-raw--codes)))
    (let ((map (nova-rime-raw--index-map nova-rime-raw--codes codes)))
      (cl-flet ((span (start end)
                  ;; 整段都要能映射，且挪动后仍然连续。
                  (let ((s (funcall map start)) (e (funcall map (1- end))))
                    (and s e (= (- e s) (- end 1 start)) (cons s (1+ e))))))
        (setq nova-rime--pins
              (cl-loop for (i char code) in nova-rime--pins
                       for j = (funcall map i)
                       when (and j (equal (nth j codes) code)) collect (list j char code)))
        (setq nova-rime-tree--frozen
              (cl-loop for (start end text keys) in nova-rime-tree--frozen
                       for new = (span start end)
                       when new collect (list (car new) (cdr new) text keys)))
        (setq nova-rime-tree--blocks
              (cl-loop for block in nova-rime-tree--blocks
                       for new = (span (nova-rime-tree-block-start block) (nova-rime-tree-block-end block))
                       when new collect (let ((copy (copy-nova-rime-tree-block block)))
                                          (setf (nova-rime-tree-block-start copy) (car new)
                                                (nova-rime-tree-block-end copy) (cdr new))
                                          copy)))))))

;;; 解码与显示

(defun nova-rime-raw--focus-syllable ()
  "光标前一个字母所在的音节；光标在末尾时没有焦点。"
  (when (and (nova-rime-raw--inside-p) (< (point) nova-rime-raw--end))
    (let ((offset (- (point) nova-rime-raw--start)) (position 0) (index 0) result)
      (dolist (unit (nova-rime--parse (nova-rime-raw--text)) (or result 0))
        (let ((next (+ position (length (car unit)) (length (cdr unit)))))
          (when (and (null result) (> offset position) (<= offset next))
            (setq result index))
          (setq position next index (1+ index)))))))

(defun nova-rime-raw--tree-p (units)
  "有完整读法；最后半个音节不算，前面的完整音节解得出来就行。"
  (and (not nova-rime--list-view) nova-rime--candidate-records
       (= (nova-rime-candidate-end (car nova-rime--candidate-records))
          (if (nova-rime-tree--pending-p units) (1- (length units)) (length units)))))

(defun nova-rime-raw--color-letters (units)
  "每个词块的字母用词块的颜色画下划线，光标所在的词块整段高亮，
这样能看出哪几个字母对应哪个词，打错时知道该改哪里。"
  (let ((offsets (cl-loop with position = 0 for unit in units
                          collect position
                          do (cl-incf position (+ (length (car unit)) (length (cdr unit))))))
        (focus nova-rime-tree--focus)
        overlays)
    (cl-loop for block in nova-rime-tree--blocks for i from 0
             for start = (nth (nova-rime-tree-block-start block) offsets)
             for end = (nth (nova-rime-tree-block-end block) offsets)
             when start do
             (let* ((face (nova-rime-tree--face block (eql i focus)))
                    (overlay (make-overlay (+ nova-rime-raw--start start)
                                           (if end (+ nova-rime-raw--start end) nova-rime-raw--end))))
               (overlay-put overlay 'face
                            (if (eql i focus)
                                face
                              `(:underline (:color ,(face-background face nil t) :position t))))
               (overlay-put overlay 'priority 1)
               (push overlay overlays)))
    overlays))

(defun nova-rime-raw--display ()
  "下划线标出正在输入的字母，下面画树；TAB 切到的整句列表画成一行。"
  (nova-rime-raw--clear-display)
  (let ((underline (make-overlay nova-rime-raw--start nova-rime-raw--end nil nil t))
        (units (nova-rime--parse nova-rime--input))
        (last (1- nova-rime-raw--end)))
    (overlay-put underline 'face 'nova-rime-raw-face)
    (push underline nova-rime-raw--overlays)
    (cond
     ((nova-rime-raw--tree-p units)
      (let ((nova-rime-tree--focus-syllable-function #'nova-rime-raw--focus-syllable))
        (setq nova-rime-raw--overlays
              (append (nova-rime-tree--present (car nova-rime--candidates) nova-rime-raw--start last)
                      (and nova-rime-raw-color-letters (nova-rime-raw--color-letters units))
                      nova-rime-raw--overlays))))
     ((and nova-rime-show-candidates nova-rime--candidates)
      (setq nova-rime--span-choices nil)
      (let ((overlay (make-overlay (save-excursion (goto-char last) (line-end-position))
                                   (save-excursion (goto-char last) (line-end-position)))))
        (overlay-put overlay 'after-string (concat "\n" (nova-rime--format-list nova-rime--candidates -1)))
        (push overlay nova-rime-raw--overlays))))))

(defun nova-rime-raw--valid-p (text)
  (string-match-p "\\`[a-z][a-zA-Z;]*\\'" text))

(defun nova-rime-raw--decode (text)
  "TEXT 的候选。最后一个音节没打完时只解前面的完整音节，
半个音节不参与，所以它不会打乱前面的整句，前面改了字母也照样重新解码。"
  (let* ((units (nova-rime--parse text))
         (pending (and (nova-rime-tree--pending-p units) (> (length units) 1) (car (last units))))
         (prefix (if pending
                     (substring text 0 (- (length text) (length (car pending)) (length (cdr pending))))
                   text)))
    (let ((nova-rime--input prefix))
      (nova-rime--candidates prefix))))

(defun nova-rime-raw--refresh ()
  "按 buffer 里的字母重新解码并显示。字母删光或混进别的字时结束输入。"
  (setq nova-rime-raw--dirty nil)
  (let ((text (and (nova-rime-raw--active-p) (nova-rime-raw--text))))
    (if (not (and text (nova-rime-raw--valid-p text)))
        (nova-rime-raw--stop)
      (let ((codes (mapcar #'car (nova-rime--parse text))))
        (nova-rime-raw--remap codes)
        (setq nova-rime-raw--codes codes
              nova-rime--input text
              nova-rime--candidates (nova-rime-raw--decode text))
        (unless nova-rime--pins (setq nova-rime--default-reading (car nova-rime--candidates)))
        (nova-rime-raw--display)))))

;;; 按键

(defun nova-rime-raw--insert (key)
  "在光标处插入一个编码键；光标不在输入里时先把上一段上屏，再开始新的一段。"
  (unless (nova-rime-raw--inside-p)
    (when (nova-rime-raw--active-p) (nova-rime-raw-commit))
    (nova-rime-raw--begin))
  (insert (char-to-string key))
  (nova-rime-raw--refresh))

(defun nova-rime-raw--select (key)
  "数字键：树上的编号改对应的词；整句列表里按序号上屏。"
  (let ((index (nova-rime--digit-index key)))
    (if nova-rime--list-view
        (when-let* ((text (nth index nova-rime--candidates)))
          (nova-rime-raw--commit-text text))
      (when-let* ((choice (nth index nova-rime--span-choices)))
        (nova-rime-tree--cycle-restore)
        (setq nova-rime-tree--cycle nil)
        (nova-rime-raw--apply choice)))))

(defun nova-rime-raw--apply (choice)
  "把 CHOICE (START END TEXT [SPLIT]) 固定下来，其余重新解码。"
  (pcase-let ((`(,start ,end ,text ,split) choice))
    (nova-rime-tree--choose-span start end split)
    (let ((units (nova-rime--parse nova-rime--input)))
      (setq nova-rime--pins
            (append (cl-loop for i from start below end
                             collect (list i (aref text (- i start)) (car (nth i units))))
                    (seq-remove (lambda (pin) (and (<= start (car pin)) (< (car pin) end)))
                                nova-rime--pins))))
    (nova-rime-raw--refresh)))

(defun nova-rime-raw--focused-p ()
  "光标停在某个词上：上下键换词、翻页键翻备选。"
  (and (nova-rime-raw--active-p) (nova-rime-raw--inside-p) (not nova-rime--list-view) nova-rime-tree--focus))

(defun nova-rime-raw--cycle (delta)
  (when nova-rime-raw--dirty (nova-rime-raw--refresh))
  (pcase (nova-rime-tree--cycle-step delta)
    ('nil nil)
    ('t (nova-rime-raw--refresh))
    (choice (nova-rime-raw--apply choice))))

(defun nova-rime-raw-next-alternative ()
  "↓：光标所在的词换成下一个备选，句子马上更新。"
  (interactive)
  (nova-rime-raw--cycle 1))

(defun nova-rime-raw-previous-alternative ()
  "↑：换回上一个备选；一路按回去回到原来的词。"
  (interactive)
  (nova-rime-raw--cycle -1))

(defun nova-rime-raw--page (delta)
  (when nova-rime-raw--dirty (nova-rime-raw--refresh))
  (when (nova-rime-tree--turn-page delta)
    (nova-rime-raw--display)))

(defun nova-rime-raw-next-page ()
  "光标所在的词的备选翻到下一页。"
  (interactive)
  (nova-rime-raw--page 1))

(defun nova-rime-raw-previous-page ()
  "光标所在的词的备选翻到上一页。"
  (interactive)
  (nova-rime-raw--page -1))

(defun nova-rime-raw--commit-text (text &optional rest)
  "把输入换成 TEXT 上屏，nova-rime 照常记录和学习；REST 是留下来继续输入的字母。"
  (let ((start (marker-position nova-rime-raw--start)))
    ;; 删除要让修改钩子照常运行：Org 的 element 缓存靠它们跟踪位置，
    ;; 瞒过它们会让缓存错位，之后 `org-element-at-point' 可能死循环。
    (delete-region nova-rime-raw--start nova-rime-raw--end)
    (goto-char start)
    (nova-rime-raw--clear-display)
    (nova-rime--commit text)
    (nova-rime-raw--stop)
    (when (and rest (not (string-empty-p rest)))
      (nova-rime-raw--begin)
      (insert rest)
      (nova-rime-raw--refresh))))

(defun nova-rime-raw-commit ()
  "空格：整串换成汉字。最后半个音节不上屏，留下来继续输入。"
  (interactive)
  (when (nova-rime-raw--active-p)
    (when nova-rime-raw--dirty (nova-rime-raw--refresh))
    (let* ((text (nova-rime-raw--text))
           (units (nova-rime--parse text))
           (pending (and (nova-rime-tree--pending-p units) (car (last units))))
           (rest (and pending (concat (car pending) (cdr pending))))
           (complete (if rest (substring text 0 (- (length text) (length rest))) text)))
      (if (string-empty-p complete)
          (nova-rime-raw--stop)
        (setq nova-rime--input complete nova-rime--candidates (nova-rime--candidates complete))
        (if (and nova-rime--candidate-records
                 (= (nova-rime-candidate-end (car nova-rime--candidate-records)) (length (nova-rime--parse complete))))
            (nova-rime-raw--commit-text (car nova-rime--candidates) rest)
          ;; 解不出来（比如中间删掉一个字母，音节对不齐了）：字母原样留下。
          (nova-rime-raw--stop))))))

(defun nova-rime-raw-commit-letters ()
  "回车：字母原样留下，结束输入。"
  (interactive)
  (nova-rime-raw--stop))

(defun nova-rime-raw-escape ()
  "ESC：字母原样留下；在 Evil 插入状态里同时退出。"
  (interactive)
  (nova-rime-raw--stop)
  (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-normal-state))
    (evil-normal-state)))

(defun nova-rime-raw-toggle-list ()
  "TAB：在树和整句列表之间切换。"
  (interactive)
  (setq nova-rime--list-view (not nova-rime--list-view))
  (nova-rime-raw--display))

(defun nova-rime-raw--input (key)
  "输入法入口。返回要原样插入的键，或者 nil 表示已经处理。"
  (let ((active (nova-rime-raw--active-p)))
    (cond
     ((nova-rime--text-read-only-p) (list key))
     (nova-rime-english-mode
      (when active (nova-rime-raw-commit))
      (list key))
     ((or (and (nova-rime--code-char-p key) (not (nova-rime--aux-char-p key))
               (or (nova-rime-raw--inside-p) (nova-rime--code-char-p key 'initial)))
          (and (nova-rime--aux-char-p key) (nova-rime-raw--inside-p) (> (point) nova-rime-raw--start)))
      (nova-rime-raw--insert key)
      nil)
     ((and active (nova-rime--digit-p key))
      (when nova-rime-raw--dirty (nova-rime-raw--refresh))
      (nova-rime-raw--select key)
      nil)
     ((and active (= key ?\s))
      (nova-rime-raw-commit)
      nil)
     ;; 光标停在词上时 - = , . 翻这个词的备选；在末尾时照常出标点。
     ((and (memq key '(?- ?= ?, ?.)) (nova-rime-raw--focused-p))
      (nova-rime-raw--page (if (memq key '(?- ?,)) -1 1))
      nil)
     ((nova-rime--punctuation-for key)
      (when active (nova-rime-raw-commit))
      (insert (nova-rime--punctuation-for key))
      nil)
     (t
      (when active (nova-rime-raw-commit))
      (list key)))))

(defun nova-rime-raw--post-command ()
  "改过字母就重新解码；光标在里面移动时焦点会变，重画；光标离开输入就上屏。
字母全删光时起止重合，不算在输入，但树和状态还在，要结束掉。"
  (when (and (markerp nova-rime-raw--start) (marker-buffer nova-rime-raw--start)
             (not (nova-rime-raw--active-p)))
    (nova-rime-raw--stop))
  ;; 换词只在连续按上下键时继续；按了别的键，下次从新的状态重新开始。
  (unless (memq this-command '(nova-rime-raw-next-alternative nova-rime-raw-previous-alternative))
    (setq nova-rime-tree--cycle nil))
  (when (nova-rime-raw--active-p)
    (cond
     ((not (nova-rime-raw--inside-p)) (nova-rime-raw-commit))
     (nova-rime-raw--dirty (nova-rime-raw--refresh))
     ((and nova-rime-tree--blocks
           (not (eql (let ((nova-rime-tree--focus-syllable-function #'nova-rime-raw--focus-syllable))
                       (nova-rime-tree--focus-index nova-rime-tree--blocks))
                     nova-rime-tree--focus)))
      (nova-rime-raw--display)))))

(defvar nova-rime-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nova-rime-raw-commit-letters)
    (define-key map (kbd "<return>") #'nova-rime-raw-commit-letters)
    (define-key map (kbd "<escape>") #'nova-rime-raw-escape)
    (define-key map (kbd "TAB") #'nova-rime-raw-toggle-list)
    ;; 只在光标停在词上时生效，否则照常移动。
    (dolist (binding '(("<down>" . nova-rime-raw-next-alternative)
                       ("<up>" . nova-rime-raw-previous-alternative)
                       ("<next>" . nova-rime-raw-next-page)
                       ("<prior>" . nova-rime-raw-previous-page)))
      (define-key map (kbd (car binding))
                  `(menu-item "" ,(cdr binding)
                              :filter ,(lambda (command) (and (nova-rime-raw--focused-p) command)))))
    map)
  "输入中额外的按键。字母、数字、空格走输入法，其余编辑键都是 Emacs 原本的命令。")

(defvar-local nova-rime-raw--emulation-alist nil)

(define-minor-mode nova-rime-raw-mode
  "原始字母输入进行中。按键优先于 Meow 等模式的键表。"
  :lighter nil
  (if nova-rime-raw-mode
      (progn
        (setq nova-rime-raw--emulation-alist `((nova-rime-raw-mode . ,nova-rime-raw-mode-map)))
        (add-to-list 'emulation-mode-map-alists 'nova-rime-raw--emulation-alist))
    (setq nova-rime-raw--emulation-alist nil)))

(defun nova-rime-raw--reset ()
  "nova-rime 自己放弃输入时（比如关掉输入法），字母留下，状态清掉。"
  (when (nova-rime-raw--active-p)
    (let ((nova-rime--input nil)) (nova-rime-raw--stop))))


(provide 'nova-rime-raw)
;;; nova-rime-raw.el ends here
