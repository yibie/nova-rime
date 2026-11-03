;;; wime-raw.el --- Keep raw letters in the buffer while composing -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 和微信输入法、鼠须管一样：输入中 buffer 里放的就是敲下的字母，汉字只出现在下面的
;; 词块里，按空格才整串换成汉字。字母是普通文字，光标可以停在任意字母前后，用普通的
;; Emacs 命令插入、删除；每条命令之后整串重新解码。
;;
;; 解码、冻结、编号和画树都用 wime-tree。这里只管：哪段字母属于输入、按键怎么分派、
;; 在中间改字母后已选的字怎么跟着挪位置、空格怎么上屏。
;;
;; 不用 wime 的半上屏（`wime-edit-mode'）：上屏后的汉字不再带读音，事后不能按读音改。
;; `wime-raw-input' 设为 nil 退回半上屏。

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'wime-tree)

(defvar wime--input)
(defvar wime--pins)
(defvar wime--candidates)
(defvar wime--candidate-records)
(defvar wime--span-choices)
(defvar wime--list-view)
(defvar wime--default-reading)
(defvar wime-english-mode)
(defvar wime-show-candidates)
(defvar wime-reset-hook)
(declare-function wime--parse "wime")
(declare-function wime--candidates "wime")
(declare-function wime--commit "wime")
(declare-function wime--reset "wime")
(declare-function wime--code-char-p "wime")
(declare-function wime--aux-char-p "wime")
(declare-function wime--digit-p "wime")
(declare-function wime--digit-index "wime")
(declare-function wime--punctuation-for "wime")
(declare-function wime--text-read-only-p "wime")
(declare-function wime--format-list "wime")
(declare-function wime-candidate-end "wime")
(declare-function evil-normal-state "evil-states" (&optional arg))

(defcustom wime-raw-input nil
  "非 nil 时输入中 buffer 里是原始字母，空格才换成汉字；nil 用 wime 的半上屏
（`wime-editable-preedit'）或浮动候选。在开启输入法之前设置。"
  :type 'boolean :group 'wime)

(defcustom wime-raw-color-letters t
  "非 nil 时，字母按对应词块的颜色画下划线，光标所在词块的字母高亮。"
  :type 'boolean :group 'wime)

(defface wime-raw-face
  '((t :underline t))
  "正在输入的字母。" :group 'wime)

(defvar-local wime-raw--start nil "输入开头的 marker。")
(defvar-local wime-raw--end nil "输入结尾的 marker，在它前面插入会跟着后移。")
(defvar-local wime-raw--overlays nil "下划线和树。")
(defvar-local wime-raw--codes nil "上一次解码时每个音节的双拼码，用来在中间改字母后挪动已选的字。")
(defvar-local wime-raw--dirty nil "输入里的字母改过，命令结束后要重新解码。")

;;; 输入段

(defun wime-raw--active-p ()
  (and (markerp wime-raw--start) (eq (marker-buffer wime-raw--start) (current-buffer))
       (< wime-raw--start wime-raw--end)))

(defun wime-raw--text ()
  (buffer-substring-no-properties wime-raw--start wime-raw--end))

(defun wime-raw--inside-p ()
  (and (wime-raw--active-p) (<= wime-raw--start (point) wime-raw--end)))

(defun wime-raw--clear-display ()
  (mapc #'delete-overlay wime-raw--overlays)
  (setq wime-raw--overlays nil))

(defun wime-raw--stop ()
  "结束输入：字母原样留在 buffer 里。"
  (wime-raw--clear-display)
  (when (markerp wime-raw--start) (set-marker wime-raw--start nil))
  (when (markerp wime-raw--end) (set-marker wime-raw--end nil))
  (setq wime-raw--start nil wime-raw--end nil wime-raw--codes nil
        wime-raw--dirty nil)
  (remove-hook 'after-change-functions #'wime-raw--after-change t)
  (wime-raw-mode -1)
  ;; `wime--reset' 会再跑 `wime-reset-hook'，这里先置空，避免重入。
  (when wime--input (let ((wime-reset-hook nil)) (wime--reset)))
  (wime-tree--reset))

(defun wime-raw--begin ()
  "在光标处开始一段输入。"
  (setq wime-raw--start (copy-marker (point))
        wime-raw--end (copy-marker (point) t))
  (add-hook 'after-change-functions #'wime-raw--after-change nil t)
  (wime-raw-mode 1))

(defun wime-raw--after-change (start end _old-length)
  (when (and (wime-raw--active-p)
             (<= wime-raw--start end) (<= start wime-raw--end))
    (setq wime-raw--dirty t)))

;;; 在中间改字母后，已选和冻结的字跟着挪

(defun wime-raw--index-map (old new)
  "OLD 和 NEW 两串双拼码，返回把旧音节序号映射到新序号的函数；改动范围里的返回 nil。"
  (let* ((n-old (length old)) (n-new (length new))
         (prefix (cl-loop for a in old for b in new while (equal a b) count t))
         (suffix (cl-loop for a in (reverse old) for b in (reverse new)
                          for k from 0 below (- (min n-old n-new) prefix)
                          while (equal a b) count t)))
    (lambda (i)
      (cond ((< i prefix) i)
            ((>= i (- n-old suffix)) (+ i (- n-new n-old)))))))

(defun wime-raw--remap (codes)
  "输入从 `wime-raw--codes' 变成 CODES：挪动或丢弃已选的字、冻结的词和上一次的词块。"
  (when (and wime-raw--codes (not (equal codes wime-raw--codes)))
    (let ((map (wime-raw--index-map wime-raw--codes codes)))
      (cl-flet ((span (start end)
                  ;; 整段都要能映射，且挪动后仍然连续。
                  (let ((s (funcall map start)) (e (funcall map (1- end))))
                    (and s e (= (- e s) (- end 1 start)) (cons s (1+ e))))))
        (setq wime--pins
              (cl-loop for (i char code) in wime--pins
                       for j = (funcall map i)
                       when (and j (equal (nth j codes) code)) collect (list j char code)))
        (setq wime-tree--frozen
              (cl-loop for (start end text keys) in wime-tree--frozen
                       for new = (span start end)
                       when new collect (list (car new) (cdr new) text keys)))
        (setq wime-tree--blocks
              (cl-loop for block in wime-tree--blocks
                       for new = (span (wime-tree-block-start block) (wime-tree-block-end block))
                       when new collect (let ((copy (copy-wime-tree-block block)))
                                          (setf (wime-tree-block-start copy) (car new)
                                                (wime-tree-block-end copy) (cdr new))
                                          copy)))))))

;;; 解码与显示

(defun wime-raw--focus-syllable ()
  "光标前一个字母所在的音节；光标在末尾时没有焦点。"
  (when (and (wime-raw--inside-p) (< (point) wime-raw--end))
    (let ((offset (- (point) wime-raw--start)) (position 0) (index 0) result)
      (dolist (unit (wime--parse (wime-raw--text)) (or result 0))
        (let ((next (+ position (length (car unit)) (length (cdr unit)))))
          (when (and (null result) (> offset position) (<= offset next))
            (setq result index))
          (setq position next index (1+ index)))))))

(defun wime-raw--tree-p (units)
  "有完整读法；最后半个音节不算，前面的完整音节解得出来就行。"
  (and (not wime--list-view) wime--candidate-records
       (= (wime-candidate-end (car wime--candidate-records))
          (if (wime-tree--pending-p units) (1- (length units)) (length units)))))

(defun wime-raw--color-letters (units)
  "每个词块的字母用词块的颜色画下划线，光标所在的词块整段高亮，
这样能看出哪几个字母对应哪个词，打错时知道该改哪里。"
  (let ((offsets (cl-loop with position = 0 for unit in units
                          collect position
                          do (cl-incf position (+ (length (car unit)) (length (cdr unit))))))
        (focus wime-tree--focus)
        overlays)
    (cl-loop for block in wime-tree--blocks for i from 0
             for start = (nth (wime-tree-block-start block) offsets)
             for end = (nth (wime-tree-block-end block) offsets)
             when start do
             (let* ((face (wime-tree--face block (eql i focus)))
                    (overlay (make-overlay (+ wime-raw--start start)
                                           (if end (+ wime-raw--start end) wime-raw--end))))
               (overlay-put overlay 'face
                            (if (eql i focus)
                                face
                              `(:underline (:color ,(face-background face nil t) :position t))))
               (overlay-put overlay 'priority 1)
               (push overlay overlays)))
    overlays))

(defun wime-raw--display ()
  "下划线标出正在输入的字母，下面画树；TAB 切到的整句列表画成一行。"
  (wime-raw--clear-display)
  (let ((underline (make-overlay wime-raw--start wime-raw--end nil nil t))
        (units (wime--parse wime--input))
        (last (1- wime-raw--end)))
    (overlay-put underline 'face 'wime-raw-face)
    (push underline wime-raw--overlays)
    (cond
     ((wime-raw--tree-p units)
      (let ((wime-tree--focus-syllable-function #'wime-raw--focus-syllable))
        (setq wime-raw--overlays
              (append (wime-tree--present (car wime--candidates) wime-raw--start last)
                      (and wime-raw-color-letters (wime-raw--color-letters units))
                      wime-raw--overlays))))
     ((and wime-show-candidates wime--candidates)
      (setq wime--span-choices nil)
      (let ((overlay (make-overlay (save-excursion (goto-char last) (line-end-position))
                                   (save-excursion (goto-char last) (line-end-position)))))
        (overlay-put overlay 'after-string (concat "\n" (wime--format-list wime--candidates -1)))
        (push overlay wime-raw--overlays))))))

(defun wime-raw--valid-p (text)
  (string-match-p "\\`[a-z][a-zA-Z;]*\\'" text))

(defun wime-raw--decode (text)
  "TEXT 的候选。最后一个音节没打完时只解前面的完整音节，
半个音节不参与，所以它不会打乱前面的整句，前面改了字母也照样重新解码。"
  (let* ((units (wime--parse text))
         (pending (and (wime-tree--pending-p units) (> (length units) 1) (car (last units))))
         (prefix (if pending
                     (substring text 0 (- (length text) (length (car pending)) (length (cdr pending))))
                   text)))
    (let ((wime--input prefix))
      (wime--candidates prefix))))

(defun wime-raw--refresh ()
  "按 buffer 里的字母重新解码并显示。字母删光或混进别的字时结束输入。"
  (setq wime-raw--dirty nil)
  (let ((text (and (wime-raw--active-p) (wime-raw--text))))
    (if (not (and text (wime-raw--valid-p text)))
        (wime-raw--stop)
      (let ((codes (mapcar #'car (wime--parse text))))
        (wime-raw--remap codes)
        (setq wime-raw--codes codes
              wime--input text
              wime--candidates (wime-raw--decode text))
        (unless wime--pins (setq wime--default-reading (car wime--candidates)))
        (wime-raw--display)))))

;;; 按键

(defun wime-raw--insert (key)
  "在光标处插入一个编码键；光标不在输入里时先把上一段上屏，再开始新的一段。"
  (unless (wime-raw--inside-p)
    (when (wime-raw--active-p) (wime-raw-commit))
    (wime-raw--begin))
  (insert (char-to-string key))
  (wime-raw--refresh))

(defun wime-raw--select (key)
  "数字键：树上的编号改对应的词；整句列表里按序号上屏。"
  (let ((index (wime--digit-index key)))
    (if wime--list-view
        (when-let* ((text (nth index wime--candidates)))
          (wime-raw--commit-text text))
      (when-let* ((choice (nth index wime--span-choices)))
        (wime-tree--cycle-restore)
        (setq wime-tree--cycle nil)
        (wime-raw--apply choice)))))

(defun wime-raw--apply (choice)
  "把 CHOICE (START END TEXT [SPLIT]) 固定下来，其余重新解码。"
  (pcase-let ((`(,start ,end ,text ,split) choice))
    (wime-tree--choose-span start end split)
    (let ((units (wime--parse wime--input)))
      (setq wime--pins
            (append (cl-loop for i from start below end
                             collect (list i (aref text (- i start)) (car (nth i units))))
                    (seq-remove (lambda (pin) (and (<= start (car pin)) (< (car pin) end)))
                                wime--pins))))
    (wime-raw--refresh)))

(defun wime-raw--focused-p ()
  "光标停在某个词上：上下键换词、翻页键翻备选。"
  (and (wime-raw--active-p) (wime-raw--inside-p) (not wime--list-view) wime-tree--focus))

(defun wime-raw--cycle (delta)
  (when wime-raw--dirty (wime-raw--refresh))
  (pcase (wime-tree--cycle-step delta)
    ('nil nil)
    ('t (wime-raw--refresh))
    (choice (wime-raw--apply choice))))

(defun wime-raw-next-alternative ()
  "↓：光标所在的词换成下一个备选，句子马上更新。"
  (interactive)
  (wime-raw--cycle 1))

(defun wime-raw-previous-alternative ()
  "↑：换回上一个备选；一路按回去回到原来的词。"
  (interactive)
  (wime-raw--cycle -1))

(defun wime-raw--page (delta)
  (when wime-raw--dirty (wime-raw--refresh))
  (when (wime-tree--turn-page delta)
    (wime-raw--display)))

(defun wime-raw-next-page ()
  "光标所在的词的备选翻到下一页。"
  (interactive)
  (wime-raw--page 1))

(defun wime-raw-previous-page ()
  "光标所在的词的备选翻到上一页。"
  (interactive)
  (wime-raw--page -1))

(defun wime-raw--commit-text (text &optional rest)
  "把输入换成 TEXT 上屏，wime 照常记录和学习；REST 是留下来继续输入的字母。"
  (let ((start (marker-position wime-raw--start)))
    (let ((inhibit-modification-hooks t))
      (delete-region wime-raw--start wime-raw--end))
    (goto-char start)
    (wime-raw--clear-display)
    (wime--commit text)
    (wime-raw--stop)
    (when (and rest (not (string-empty-p rest)))
      (wime-raw--begin)
      (insert rest)
      (wime-raw--refresh))))

(defun wime-raw-commit ()
  "空格：整串换成汉字。最后半个音节不上屏，留下来继续输入。"
  (interactive)
  (when (wime-raw--active-p)
    (when wime-raw--dirty (wime-raw--refresh))
    (let* ((text (wime-raw--text))
           (units (wime--parse text))
           (pending (and (wime-tree--pending-p units) (car (last units))))
           (rest (and pending (concat (car pending) (cdr pending))))
           (complete (if rest (substring text 0 (- (length text) (length rest))) text)))
      (if (string-empty-p complete)
          (wime-raw--stop)
        (setq wime--input complete wime--candidates (wime--candidates complete))
        (if (and wime--candidate-records
                 (= (wime-candidate-end (car wime--candidate-records)) (length (wime--parse complete))))
            (wime-raw--commit-text (car wime--candidates) rest)
          ;; 解不出来（比如中间删掉一个字母，音节对不齐了）：字母原样留下。
          (wime-raw--stop))))))

(defun wime-raw-commit-letters ()
  "回车：字母原样留下，结束输入。"
  (interactive)
  (wime-raw--stop))

(defun wime-raw-escape ()
  "ESC：字母原样留下；在 Evil 插入状态里同时退出。"
  (interactive)
  (wime-raw--stop)
  (when (and (bound-and-true-p evil-local-mode) (fboundp 'evil-normal-state))
    (evil-normal-state)))

(defun wime-raw-toggle-list ()
  "TAB：在树和整句列表之间切换。"
  (interactive)
  (setq wime--list-view (not wime--list-view))
  (wime-raw--display))

(defun wime-raw--input (key)
  "输入法入口。返回要原样插入的键，或者 nil 表示已经处理。"
  (let ((active (wime-raw--active-p)))
    (cond
     ((wime--text-read-only-p) (list key))
     (wime-english-mode
      (when active (wime-raw-commit))
      (list key))
     ((or (and (wime--code-char-p key) (not (wime--aux-char-p key))
               (or (wime-raw--inside-p) (wime--code-char-p key 'initial)))
          (and (wime--aux-char-p key) (wime-raw--inside-p) (> (point) wime-raw--start)))
      (wime-raw--insert key)
      nil)
     ((and active (wime--digit-p key))
      (when wime-raw--dirty (wime-raw--refresh))
      (wime-raw--select key)
      nil)
     ((and active (= key ?\s))
      (wime-raw-commit)
      nil)
     ;; 光标停在词上时 - = , . 翻这个词的备选；在末尾时照常出标点。
     ((and (memq key '(?- ?= ?, ?.)) (wime-raw--focused-p))
      (wime-raw--page (if (memq key '(?- ?,)) -1 1))
      nil)
     ((wime--punctuation-for key)
      (when active (wime-raw-commit))
      (insert (wime--punctuation-for key))
      nil)
     (t
      (when active (wime-raw-commit))
      (list key)))))

(defun wime-raw--post-command ()
  "改过字母就重新解码；光标在里面移动时焦点会变，重画；光标离开输入就上屏。
字母全删光时起止重合，不算在输入，但树和状态还在，要结束掉。"
  (when (and (markerp wime-raw--start) (marker-buffer wime-raw--start)
             (not (wime-raw--active-p)))
    (wime-raw--stop))
  ;; 换词只在连续按上下键时继续；按了别的键，下次从新的状态重新开始。
  (unless (memq this-command '(wime-raw-next-alternative wime-raw-previous-alternative))
    (setq wime-tree--cycle nil))
  (when (wime-raw--active-p)
    (cond
     ((not (wime-raw--inside-p)) (wime-raw-commit))
     (wime-raw--dirty (wime-raw--refresh))
     ((and wime-tree--blocks
           (not (eql (let ((wime-tree--focus-syllable-function #'wime-raw--focus-syllable))
                       (wime-tree--focus-index wime-tree--blocks))
                     wime-tree--focus)))
      (wime-raw--display)))))

(defvar wime-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wime-raw-commit-letters)
    (define-key map (kbd "<return>") #'wime-raw-commit-letters)
    (define-key map (kbd "<escape>") #'wime-raw-escape)
    (define-key map (kbd "TAB") #'wime-raw-toggle-list)
    ;; 只在光标停在词上时生效，否则照常移动。
    (dolist (binding '(("<down>" . wime-raw-next-alternative)
                       ("<up>" . wime-raw-previous-alternative)
                       ("<next>" . wime-raw-next-page)
                       ("<prior>" . wime-raw-previous-page)))
      (define-key map (kbd (car binding))
                  `(menu-item "" ,(cdr binding)
                              :filter ,(lambda (command) (and (wime-raw--focused-p) command)))))
    map)
  "输入中额外的按键。字母、数字、空格走输入法，其余编辑键都是 Emacs 原本的命令。")

(defvar-local wime-raw--emulation-alist nil)

(define-minor-mode wime-raw-mode
  "原始字母输入进行中。按键优先于 Meow 等模式的键表。"
  :lighter nil
  (if wime-raw-mode
      (progn
        (setq wime-raw--emulation-alist `((wime-raw-mode . ,wime-raw-mode-map)))
        (add-to-list 'emulation-mode-map-alists 'wime-raw--emulation-alist))
    (setq wime-raw--emulation-alist nil)))

(defun wime-raw--reset ()
  "wime 自己放弃输入时（比如关掉输入法），字母留下，状态清掉。"
  (when (wime-raw--active-p)
    (let ((wime--input nil)) (wime-raw--stop))))


(provide 'wime-raw)
;;; wime-raw.el ends here
