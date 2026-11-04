;;; nova-rime-tree.el --- Tree candidates and sentence correction -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; `nova-rime-candidate-style' 设为 `tree' 时启用；`nova-rime-raw-input' 的原始字母输入也用它画树。
;;
;; 把当前读法切成词块，用连线串起来，备选挂在词块下面。和 Nova Squirrel 的改错方式一致：
;; - 不确定的词（橙、紫）直接列出备选，全句连续编号，按数字就改。
;; - 光标停在某个词里时只展开这个词，数字只选它；光标回到句尾恢复全句编号。
;; - 打过去的词冻结：不再被后文改掉。冻结只在搜索时作为软约束加进 `nova-rime--pins'，
;;   不写进读音单元，所以 nova-rime 的上屏学习看到的仍是它自己的首选。
;; - 最后一个音节没打完时，文字和树都保持上一次的样子。
;; 选字沿用 nova-rime 的 `nova-rime--span-choices' 与 `nova-rime--choose-span'，这里只决定选哪些、怎么画。

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar nova-rime--input)
(defvar nova-rime--pins)
(defvar nova-rime--candidates)
(defvar nova-rime--candidate-records)
(defvar nova-rime--span-choices)
(defvar nova-rime--list-view)
(defvar nova-rime-candidate-style)
(defvar nova-rime-show-candidates)
(defvar nova-rime-word-penalty)
(defvar nova-rime-tree-show-keys)
(defvar nova-rime-edit--start)
(defvar nova-rime-edit--end)
(defvar nova-rime-edit--units)
(defvar nova-rime-edit--correction-decorations)
(defvar nova-rime-edit--correction-layout)
(defvar nova-rime-raw-input)
(declare-function nova-rime--parse "nova-rime")
(declare-function nova-rime--span-code "nova-rime")
(declare-function nova-rime--unit-complete-p "nova-rime")
(declare-function nova-rime--entries-for "nova-rime")
(declare-function nova-rime--matching-entries "nova-rime")
(declare-function nova-rime-candidate-end "nova-rime")
(declare-function nova-rime-edit--active-p "nova-rime-edit")
(declare-function nova-rime-edit--render "nova-rime-edit")
(declare-function nova-rime-edit--syllables "nova-rime-edit")
(declare-function nova-rime-edit--unit-literal "nova-rime-edit")
(declare-function nova-rime-edit--unit-keys "nova-rime-edit")
(declare-function nova-rime-edit--unit-code "nova-rime-edit")
(declare-function nova-rime-edit--shown "nova-rime-edit")
(declare-function nova-rime-edit--offset "nova-rime-edit")
(declare-function nova-rime-edit--display-width "nova-rime-edit")
(declare-function nova-rime-edit--correction-location "nova-rime-edit")
(declare-function nova-rime-edit--layout-signature "nova-rime-edit")
(declare-function nova-rime-edit--show-corrections "nova-rime-edit")

(defface nova-rime-tree-confident-face
  '((t :background "#2f6fde" :foreground "white"))
  "有把握的词块。" :group 'nova-rime)

(defface nova-rime-tree-unsure-face
  '((t :background "#d26624" :foreground "white"))
  "没把握的词块，备选直接列出。" :group 'nova-rime)

(defface nova-rime-tree-corrected-face
  '((t :background "#7b48d8" :foreground "white"))
  "整句选了词频较低的词。" :group 'nova-rime)

(defface nova-rime-tree-pinned-face
  '((t :background "#1f9d6b" :foreground "white"))
  "用户选过的词。" :group 'nova-rime)

(defface nova-rime-tree-focus-face
  '((t :background "#f5c518" :foreground "black"))
  "光标所在的词。" :group 'nova-rime)

(defface nova-rime-tree-line-face
  '((t :strike-through "#8a8a8a"))
  "连接词块的线。" :group 'nova-rime)

(defface nova-rime-tree-branch-face
  '((t :foreground "#8a8a8a"))
  "备选前的分支符号。" :group 'nova-rime)

(defface nova-rime-tree-current-face
  '((t :background "#2f6fde" :foreground "white"))
  "上下键换词时，句子里正在用的那一项。" :group 'nova-rime)

(defcustom nova-rime-tree-max-block 3
  "词块最多几个字。词库有「我很喜欢」这样的短语，块太长就没有东西可选。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-confident-share 80
  "占比低于它算没把握（0–100）。调高会有更多词显示成橙色、直接列出备选。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-freeze t
  "非 nil 时，打过去的多字词有把握或已稳定就冻结，后文不再改它。
nil 时每打一个音节整句都可能重新解码（rime 原本的行为）。"
  :type 'boolean :group 'nova-rime)
(defcustom nova-rime-tree-freeze-share 50
  "多字词占比达到它，打过去就冻结。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-stable-syllables 2
  "多字词之后又打了这么多音节都没变，就冻结。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-overview 'uncertain
  "没有焦点时哪些词直接列出备选、能按数字改。
`uncertain'：只有没把握的词；`all'：每个词（编号用完为止）；nil：都不列，要先移光标到词上。"
  :type '(choice (const :tag "只列没把握的词" uncertain)
                 (const :tag "每个词都列" all)
                 (const :tag "不列，只在焦点词上列" nil))
  :group 'nova-rime)
(defcustom nova-rime-tree-overview-alts 2
  "没有焦点时每个词列几个备选。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-focus-alts 6
  "焦点词每页几个备选。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-focus-pages 6
  "焦点词的备选最多翻几页。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-char-alts 4
  "光标停在多字词的某个字上时，备选先列这个字的几个同音单字，后面才是整个词的写法。
选单字只换这一个字，词里其他字不动。设为 0 不列单字。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-same-length-alts 4
  "焦点词里同长度备选最多几个，其余位置留给别的长度（用来重新切分）。"
  :type 'integer :group 'nova-rime)
(defcustom nova-rime-tree-labels 9
  "全句最多几个编号。"
  :type 'integer :group 'nova-rime)

(cl-defstruct (nova-rime-tree-block (:constructor nova-rime-tree--make-block))
  start end text keys share corrected pinned (age 0) alts)

(defvar-local nova-rime-tree--blocks nil
  "上一次画出的词块。最后一个音节没打完时继续用它。")
(defvar-local nova-rime-tree--frozen nil
  "冻结的词，(START END TEXT KEYS)。KEYS 是这几个音节的按键，对不上就作废。")
(defvar-local nova-rime-tree--syllables 0
  "上一次的完整音节数，用来判断是不是多打了一个音节。")
(defvar-local nova-rime-tree--focus nil
  "上一次画出时光标所在的词块序号，nil 表示没有焦点。")
(defvar-local nova-rime-tree--focus-syllable nil
  "上一次画出时光标所在的音节序号。")
(defvar-local nova-rime-tree--page 0 "焦点词的备选翻到第几页，从 0 开始。")
(defvar-local nova-rime-tree--page-key nil "翻页对应的 (输入 . 焦点词起点)，变了就回到第一页。")
(defvar-local nova-rime-tree--pages 1 "焦点词的备选一共几页。")
(defvar-local nova-rime-tree--current nil "本页里句子正在用的那一项（上下键换词时），从 0 开始。")
(defvar-local nova-rime-tree--cycle nil
  "上下键换词的状态，plist：:alts 第一次按键时记下的备选，:index 当前第几项（0 是原来的词），
:pins :frozen :blocks :syllables 第一次按键前的状态，每次换词都从它出发。")

;;; 数据

(defun nova-rime-tree--units ()
  (nova-rime--parse (or nova-rime--input "")))

(defun nova-rime-tree--complete-count (units)
  (cl-loop for unit in units while (nova-rime--unit-complete-p unit) count t))

(defun nova-rime-tree--keys (units start end)
  (mapcar #'car (seq-subseq units start end)))

(defun nova-rime-tree--weights (units start length)
  "从 START 起 LENGTH 个音节的词，((WORD . WEIGHT) ...)，按词频从高到低，已按辅助码过滤。"
  (let ((end (+ start length)) seen)
    (when (<= end (length units))
      (dolist (entry (nova-rime--matching-entries (nova-rime--entries-for (nova-rime--span-code units start end))
                                             (seq-subseq units start end)))
        (let ((word (car entry)) (weight (or (nth 2 entry) 0)))
          (when (and (= (length word) length) (not (assoc word seen)))
            (push (cons word weight) seen))))
      (sort (nreverse seen) (lambda (a b) (> (cdr a) (cdr b)))))))

(defun nova-rime-tree--share (weights word)
  "WORD 占 WEIGHTS 词频的比例（0-100）；不在里面时为 nil。"
  (when-let* ((own (cdr (assoc word weights))))
    (let ((total (apply #'+ (mapcar #'cdr weights))))
      (when (> total 0) (round (* 100.0 own) total)))))

(defun nova-rime-tree--word-ends (text units)
  "把 TEXT 切成词，返回每个词的结束位置。
和 nova-rime 的词格同一套打分：各词 log 词频之和，每多一个词扣 `nova-rime-word-penalty'。
只取最长词会切出「他说这」「很难解」这种词库里有、但不像人话的块。
词库里没有的单字按词频 1 算，保证总能切开。"
  (let* ((n (length text))
         (best (make-vector (1+ n) nil)))
    (aset best 0 (cons 0.0 nil))
    (dotimes (end n)
      (let ((end (1+ end)))
        (cl-loop for start from (max 0 (- end nova-rime-tree-max-block)) below end
                 for previous = (aref best start)
                 when previous do
                 (let* ((word (substring text start end))
                        (weight (cdr (assoc word (nova-rime-tree--weights units start (- end start))))))
                   (when (or weight (= (- end start) 1))
                     (let ((score (+ (car previous) (log (max 1 (or weight 1))) (- nova-rime-word-penalty))))
                       (when (or (null (aref best end)) (> score (car (aref best end))))
                         (aset best end (cons score (cons end (cdr previous)))))))))))
    (reverse (cdr (aref best n)))))

(defun nova-rime-tree--alternatives (block units limit same-length)
  "BLOCK 起点开始的其他写法：先放 SAME-LENGTH 个同长度的，再每种长度一个，最多 LIMIT 个。
每项是 (START END TEXT)，END 可以和块不同，用来重新切分。"
  (let* ((start (nova-rime-tree-block-start block))
         (length (- (nova-rime-tree-block-end block) start))
         (seen (list (nova-rime-tree-block-text block)))
         result)
    (cl-flet ((add (len count)
                (cl-loop for (word . _) in (nova-rime-tree--weights units start len)
                         while (and (> count 0) (< (length result) limit))
                         unless (member word seen)
                         do (push word seen) (push (list start (+ start len) word) result)
                         (cl-decf count))))
      (add length same-length)
      (cl-loop for other from (1- length) downto 1 do (add other 1))
      (cl-loop for other from (1+ length) to nova-rime-tree-max-block do (add other 1))
      (add length limit))
    (nreverse result)))

(defun nova-rime-tree--focus-list (block units syllable)
  "焦点词的全部备选。光标 SYLLABLE 在多字词里时，先放这个字的几个同音单字，
再放整个词的写法，剩下的单字排在后面。单字项带第四个元素 (START END TEXT)，
是它所在的词，选它时词里其他字原样冻结。"
  (let* ((limit (* nova-rime-tree-focus-alts nova-rime-tree-focus-pages))
         (start (nova-rime-tree-block-start block))
         (end (nova-rime-tree-block-end block))
         (text (nova-rime-tree-block-text block))
         (whole (nova-rime-tree--alternatives block units limit nova-rime-tree-same-length-alts))
         (split (and syllable (> nova-rime-tree-char-alts 0) (> (- end start) 1)
                     (<= start syllable) (< syllable end)
                     (list start end text)))
         (current (and split (char-to-string (aref text (- syllable start)))))
         (chars (and split
                     (cl-loop for (word . _) in (nova-rime-tree--weights units syllable 1)
                              unless (equal word current)
                              collect (list syllable (1+ syllable) word split)))))
    (if (not chars)
        whole
      (let* ((first (min nova-rime-tree-char-alts (length chars)))
             (words (max 0 (- nova-rime-tree-focus-alts first))))
        (seq-take (append (seq-take chars first) (seq-take whole words)
                          (nthcdr first chars) (nthcdr words whole))
                  limit)))))

(defun nova-rime-tree--build (text units)
  "把 TEXT 切成词块，算出比例、是否被纠正、是否用户选过、稳定了几个音节。"
  (let* ((pinned (mapcar #'car nova-rime--pins))
         (complete (nova-rime-tree--complete-count units))
         (grew (> complete nova-rime-tree--syllables))
         (start 0) blocks)
    (dolist (end (nova-rime-tree--word-ends text units))
      (let* ((word (substring text start end))
             (weights (nova-rime-tree--weights units start (- end start)))
             (share (nova-rime-tree--share weights word))
             (previous (seq-find (lambda (b) (and (= (nova-rime-tree-block-start b) start)
                                                  (= (nova-rime-tree-block-end b) end)
                                                  (equal (nova-rime-tree-block-text b) word)))
                                 nova-rime-tree--blocks)))
        ;; 一两个字的词不在词库里，多半是切错了，按 0 算。
        (when (and (not share) weights (< (- end start) 3)) (setq share 0))
        (push (nova-rime-tree--make-block
               :start start :end end :text word
               :keys (nova-rime-tree--keys units start end)
               :share share
               :corrected (and share (seq-some (lambda (w) (> (cdr w) (or (cdr (assoc word weights)) 0)))
                                               weights))
               :pinned (cl-loop for i from start below end always (memq i pinned))
               :age (if previous (+ (nova-rime-tree-block-age previous) (if grew 1 0)) 0))
              blocks)
        (setq start end)))
    (setq nova-rime-tree--syllables complete)
    (nreverse blocks)))

(defun nova-rime-tree--uncertain-p (block)
  (and (not (nova-rime-tree-block-pinned block))
       (or (nova-rime-tree-block-corrected block)
           (and (nova-rime-tree-block-share block)
                (< (nova-rime-tree-block-share block) nova-rime-tree-confident-share)))))

;;; 冻结

(defun nova-rime-tree--freeze (block)
  (let ((start (nova-rime-tree-block-start block)))
    (setq nova-rime-tree--frozen
          (cons (list start (nova-rime-tree-block-end block)
                      (nova-rime-tree-block-text block) (nova-rime-tree-block-keys block))
                (seq-remove (lambda (f) (= (car f) start)) nova-rime-tree--frozen)))))

(defun nova-rime-tree--update-frozen (blocks)
  "不是最后一块的多字词，有把握或已经稳定，就冻结。关掉 `nova-rime-tree-freeze' 时不冻结。"
  (cl-loop for block in (and nova-rime-tree-freeze (butlast blocks))
           when (and (>= (length (nova-rime-tree-block-text block)) 2)
                     (or (>= (or (nova-rime-tree-block-share block) 0) nova-rime-tree-freeze-share)
                         (>= (nova-rime-tree-block-age block) nova-rime-tree-stable-syllables))
                     (not (seq-find (lambda (f) (= (car f) (nova-rime-tree-block-start block)))
                                    nova-rime-tree--frozen)))
           do (nova-rime-tree--freeze block)))

(defun nova-rime-tree--soft-pins (units)
  "仍然有效的冻结，按 `nova-rime--pins' 的格式 (POSITION CHAR CODE)。用户选过的字优先。"
  (let ((hard (mapcar #'car nova-rime--pins)))
    (cl-loop for (start end text keys) in nova-rime-tree--frozen
             when (and (<= end (length units)) (equal keys (nova-rime-tree--keys units start end)))
             append (cl-loop for i from start below end
                             unless (memq i hard)
                             collect (list i (aref text (- i start)) (car (nth i units)))))))

(defun nova-rime-tree--search-pins (input)
  "`nova-rime--candidates' 搜索 INPUT 时用的 `nova-rime--pins'：整句搜索带上冻结的字。
别处按单个音节查词时不加。"
  (if (and nova-rime-tree--frozen (equal input nova-rime--input))
      (append nova-rime--pins (nova-rime-tree--soft-pins (nova-rime--parse input)))
    nova-rime--pins))

(defun nova-rime-tree--freeze-span (start end text units)
  (when (< start end)
    (setq nova-rime-tree--frozen
          (cons (list start end text (nova-rime-tree--keys units start end))
                (seq-remove (lambda (f) (= (car f) start)) nova-rime-tree--frozen)))))

(defun nova-rime-tree--choose-span (start end &optional split)
  "选词时只冻结所选范围之前的词块；之后的部分带着新选的字重新解码。
改了前面的字，后面的字可以跟它重新组词：显 + 时期 变成 显示器。
用户亲手选过的字在 `nova-rime--pins' 里，不管在前在后都保留。
SPLIT 是 (START END TEXT)：只换了这个词里的一个字，词里在它前面的字也冻结。"
  (setq nova-rime-tree--frozen
        (seq-remove (lambda (f) (> (nth 1 f) start)) nova-rime-tree--frozen))
  (dolist (block nova-rime-tree--blocks)
    (when (<= (nova-rime-tree-block-end block) start)
      (nova-rime-tree--freeze block)))
  (pcase split
    (`(,word-start ,_word-end ,text)
     (let ((units (nova-rime-tree--units)))
       (when (<= start (length units))
         (nova-rime-tree--freeze-span word-start start (substring text 0 (- start word-start)) units))))))

(defun nova-rime-tree--reset ()
  (setq nova-rime-tree--blocks nil nova-rime-tree--frozen nil
        nova-rime-tree--syllables 0 nova-rime-tree--focus nil nova-rime-tree--focus-syllable nil
        nova-rime-tree--page 0 nova-rime-tree--page-key nil nova-rime-tree--cycle nil))

;;; 上下键换词

(defun nova-rime-tree--snapshot ()
  (list :pins nova-rime--pins :frozen nova-rime-tree--frozen
        :blocks nova-rime-tree--blocks :syllables nova-rime-tree--syllables))

(defun nova-rime-tree--restore (state)
  (setq nova-rime--pins (plist-get state :pins)
        nova-rime-tree--frozen (plist-get state :frozen)
        nova-rime-tree--blocks (plist-get state :blocks)
        nova-rime-tree--syllables (plist-get state :syllables)))

(defun nova-rime-tree--cycle-step (delta)
  "换到焦点词的下一项（DELTA 为 1）或上一项（-1）。
第一次按时记下备选和当前状态，之后每次都从这个状态出发，所以一路按回去能回到原来的词。
返回要套用的备选；回到原来的词时返回 t；到头了返回 nil。"
  (unless nova-rime-tree--cycle
    (when-let* ((focus nova-rime-tree--focus)
                (block (nth focus nova-rime-tree--blocks))
                (alts (nova-rime-tree--focus-list block (nova-rime-tree--units) nova-rime-tree--focus-syllable)))
      (setq nova-rime-tree--cycle (append (list :alts alts :index 0) (nova-rime-tree--snapshot)))))
  (when nova-rime-tree--cycle
    (let* ((alts (plist-get nova-rime-tree--cycle :alts))
           (index (+ (plist-get nova-rime-tree--cycle :index) delta)))
      (when (and (>= index 0) (<= index (length alts)))
        (setq nova-rime-tree--cycle (plist-put nova-rime-tree--cycle :index index))
        (nova-rime-tree--restore nova-rime-tree--cycle)
        (if (= index 0) t (nth (1- index) alts))))))

(defun nova-rime-tree--cycle-restore ()
  "上下键换词中按数字：先回到换词前的状态，再按编号选。"
  (when nova-rime-tree--cycle
    (nova-rime-tree--restore nova-rime-tree--cycle)))

(defun nova-rime-tree--turn-page (delta)
  "焦点词的备选翻页。翻不动时返回 nil。"
  (let ((page (+ nova-rime-tree--page delta)))
    (when (and nova-rime-tree--focus (>= page 0) (< page nova-rime-tree--pages))
      (setq nova-rime-tree--page page nova-rime-tree--cycle nil)
      t)))

;;; 半个音节

(defun nova-rime-tree--pending-p (units)
  "最后一个音节没打完，前面都是完整音节。
开着 `nova-rime-abbreviate' 时单个声母也算完整音节（简拼），不会没打完。"
  (and units (not (nova-rime--unit-complete-p (car (last units))))
       (= (nova-rime-tree--complete-count units) (1- (length units)))))

(defun nova-rime-tree--pending-text ()
  "最后一个音节没打完时，已经显示的字保持不动，返回这段文字；否则返回 nil。
nova-rime 原本会逐字重新猜。"
  (let ((syllables (and (nova-rime-edit--active-p) (nova-rime-edit--syllables nova-rime-edit--units))))
    (when (and syllables
             (eq nova-rime-candidate-style 'tree)
             (nova-rime-tree--pending-p (nova-rime-tree--units))
             (cl-every #'nova-rime-edit--shown (butlast syllables)))
        (concat (mapconcat #'nova-rime-edit--shown (butlast syllables) "")
                (or (nova-rime-edit--shown (car (last syllables)))
                    (substring (nova-rime-edit--unit-keys (car (last syllables))) 0 1))))))

;;; 焦点与编号

(defun nova-rime-tree--edit-focus-syllable ()
  "半上屏：光标前一个字是第几个音节。光标在句尾时没有焦点。"
  (when (and (nova-rime-edit--active-p) (< (point) nova-rime-edit--end) (>= (point) nova-rime-edit--start))
    (max 0 (1- (cl-loop for unit in (seq-take nova-rime-edit--units (- (point) nova-rime-edit--start))
                        count (not (nova-rime-edit--unit-literal unit)))))))

(defvar-local nova-rime-tree--focus-syllable-function #'nova-rime-tree--edit-focus-syllable
  "返回光标所在的音节序号，没有焦点时返回 nil。半上屏和原始字母输入各有一个。")

(defun nova-rime-tree--focus-index (blocks)
  "光标所在音节所在的词块。"
  (when-let* ((syllable (setq nova-rime-tree--focus-syllable
                              (funcall nova-rime-tree--focus-syllable-function))))
    (cl-position-if (lambda (b) (and (<= (nova-rime-tree-block-start b) syllable)
                                     (< syllable (nova-rime-tree-block-end b))))
                    blocks)))

(defun nova-rime-tree--assign (blocks units focus)
  "给要显示备选的块填上 alts，并按编号顺序设置 `nova-rime--span-choices'。"
  (dolist (block blocks) (setf (nova-rime-tree-block-alts block) nil))
  (setq nova-rime-tree--pages 1 nova-rime-tree--current nil)
  (if focus
      (let* ((block (nth focus blocks))
             (cycle nova-rime-tree--cycle)
             (index (and cycle (plist-get cycle :index)))
             (all (if cycle
                      (plist-get cycle :alts)
                    (nova-rime-tree--focus-list block units nova-rime-tree--focus-syllable)))
             (per (max 1 nova-rime-tree-focus-alts))
             (key (cons nova-rime--input (nova-rime-tree-block-start block))))
        (setq nova-rime-tree--pages (max 1 (ceiling (length all) (float per))))
        (cond
         ;; 换词时翻到正在用的那一项所在的页。
         ((and cycle (> index 0))
          (setq nova-rime-tree--page (/ (1- index) per)
                nova-rime-tree--current (% (1- index) per)))
         (cycle)
         ((not (equal key nova-rime-tree--page-key))
          (setq nova-rime-tree--page 0)))
        (setq nova-rime-tree--page-key key
              nova-rime-tree--page (min nova-rime-tree--page (1- nova-rime-tree--pages)))
        (setf (nova-rime-tree-block-alts block)
              (seq-take (nthcdr (* nova-rime-tree--page per) all) per)))
    ;; 编号不够时最不确定的先拿，每块要么全有要么没有。
    (let ((labels 0))
      (dolist (block (sort (pcase nova-rime-tree-overview
                             ('all (seq-remove #'nova-rime-tree-block-pinned blocks))
                             ('uncertain (seq-filter #'nova-rime-tree--uncertain-p blocks)))
                           (lambda (a b) (< (or (nova-rime-tree-block-share a) 0)
                                            (or (nova-rime-tree-block-share b) 0)))))
        (let ((alts (nova-rime-tree--alternatives block units nova-rime-tree-overview-alts
                                                  nova-rime-tree-overview-alts)))
          (when (and alts (<= (+ labels (length alts)) nova-rime-tree-labels))
            (setf (nova-rime-tree-block-alts block) alts)
            (cl-incf labels (length alts)))))))
  (setq nova-rime--span-choices (cl-loop for block in blocks append (nova-rime-tree-block-alts block))))

;;; 显示

(defun nova-rime-tree--face (block focused)
  (cond (focused 'nova-rime-tree-focus-face)
        ((nova-rime-tree-block-pinned block) 'nova-rime-tree-pinned-face)
        ((nova-rime-tree-block-corrected block) 'nova-rime-tree-corrected-face)
        ((nova-rime-tree--uncertain-p block) 'nova-rime-tree-unsure-face)
        (t 'nova-rime-tree-confident-face)))

(defun nova-rime-tree--align (column &optional face)
  (propertize " " 'display `(space :align-to ,column) 'face face))

(defun nova-rime-tree--chunk (columns)
  "COLUMNS 是 (CHIP-X CHIP CELLS-X CELLS)：一排词块和挂在下面的备选。"
  (let ((height (apply #'max 0 (mapcar (lambda (c) (length (nth 3 c))) columns))))
    (concat
     "\n"
     (cl-loop for (x chip) in columns
              concat (concat (nova-rime-tree--align x 'nova-rime-tree-line-face) chip))
     (mapconcat (lambda (row)
                  (concat "\n" (cl-loop for (_x _chip cells-x cells) in columns
                                        for cell = (nth row cells)
                                        when cell concat (concat (nova-rime-tree--align cells-x) cell))))
                (number-sequence 0 (1- height)) ""))))

(defun nova-rime-tree--rows (blocks focus left window)
  "从 LEFT 开始画词块。词块只按自身宽度排，备选放不下时往右让，不推动词块。"
  (let* ((limit (if window
                    (- (window-body-width window)
                       (with-selected-window window (line-number-display-width 'columns)) 1)
                  most-positive-fixnum))
         (gap (nova-rime-edit--display-width "──" window))
         (branch (propertize "╰" 'face 'nova-rime-tree-branch-face))
         (x (max left 0)) (floor 0) (label 0) chunks chunk)
    (cl-loop for block in blocks for i from 0 do
             (let* ((chip (propertize (concat " " (nova-rime-tree-block-text block) " ")
                                      'face (nova-rime-tree--face block (eql i focus))))
                    (width (nova-rime-edit--display-width chip window))
                    (focused (eql i focus))
                    (cells (cl-loop for alt in (nova-rime-tree-block-alts block) for j from 0
                                    collect
                                    (concat branch
                                            (propertize (number-to-string (% (cl-incf label) 10))
                                                        'face 'nova-rime-label-face)
                                            (propertize (nth 2 alt) 'face
                                                        (if (and focused (eql j nova-rime-tree--current))
                                                            'nova-rime-tree-current-face
                                                          'nova-rime-alternatives-face))))))
               (when (and focused cells (> nova-rime-tree--pages 1))
                 (setcar (last cells)
                         (concat (car (last cells))
                                 (propertize (format " ‹ %d/%d ›" (1+ nova-rime-tree--page) nova-rime-tree--pages)
                                             'face 'nova-rime-tree-branch-face))))
               (when (and chunk (> (+ x width) limit))
                 (push (nreverse chunk) chunks)
                 (setq chunk nil x 0 floor 0))
               (let ((cells-x (max x floor)))
                 (push (list x chip cells-x cells) chunk)
                 (when cells
                   (setq floor (+ cells-x gap (apply #'max (mapcar (lambda (c) (nova-rime-edit--display-width c window))
                                                                   cells))))))
               (setq x (+ x width gap))))
    (when chunk (push (nreverse chunk) chunks))
    (mapconcat #'nova-rime-tree--chunk (nreverse chunks) "")))

(defun nova-rime-tree--draw (blocks focus start last)
  "在 START 到 LAST 这段文字下面画树，返回画出的 overlay。LAST 是最后一个字的位置。"
  (let ((windows (or (get-buffer-window-list (current-buffer) nil t) '(nil))) overlays)
    (dolist (window windows overlays)
      (pcase-let* ((`(,start-anchor ,start-column ,_) (nova-rime-edit--correction-location start window))
                   (`(,anchor ,_ ,soft) (nova-rime-edit--correction-location last window))
                   (left (if (eq start-anchor anchor) start-column 0))
                   (overlay (make-overlay anchor anchor)))
        (when window (overlay-put overlay 'window window))
        (overlay-put overlay 'after-string
                     (concat (nova-rime-tree--rows blocks focus left window) (when soft "\n")))
        (push overlay overlays)))))

(defun nova-rime-tree--present (text start last)
  "按 TEXT 重新切块，选出备选，画在 START 到 LAST 下面。返回画出的 overlay。
原始字母输入时最后半个音节不参与，TEXT 只覆盖前面的完整音节；
半上屏时沿用上一次的词块。"
  (let* ((all (nova-rime-tree--units))
         (pending (nova-rime-tree--pending-p all))
         (units (if pending (butlast all) all))
         (blocks (cond ((not pending) (nova-rime-tree--build text units))
                       ((and nova-rime-raw-input (= (length text) (length units)))
                        (nova-rime-tree--build text units))
                       (t nova-rime-tree--blocks)))
         (focus (nova-rime-tree--focus-index blocks)))
    (unless pending (nova-rime-tree--update-frozen blocks))
    (setq nova-rime-tree--blocks blocks nova-rime-tree--focus focus)
    (nova-rime-tree--assign blocks units focus)
    (when nova-rime-show-candidates
      (nova-rime-tree--draw blocks focus start last))))

(defun nova-rime-tree--show (text)
  "半上屏：画树，按键没替换文字时给有备选的词块加波浪线。"
  (let ((overlays (nova-rime-tree--present
                   text nova-rime-edit--start
                   (+ nova-rime-edit--start (nova-rime-edit--offset (1- (length text)))))))
    (dolist (block (unless (memq nova-rime-tree-show-keys '(inline t)) nova-rime-tree--blocks))
      (when (nova-rime-tree-block-alts block)
        (let ((overlay (make-overlay (+ nova-rime-edit--start (nova-rime-edit--offset (nova-rime-tree-block-start block)))
                                     (+ nova-rime-edit--start 1 (nova-rime-edit--offset (1- (nova-rime-tree-block-end block)))))))
          (overlay-put overlay 'face 'nova-rime-uncertain-face)
          (overlay-put overlay 'priority 1)
          (push overlay overlays))))
    (setq nova-rime-edit--correction-decorations (append overlays nova-rime-edit--correction-decorations)))
  (setq nova-rime-edit--correction-layout (nova-rime-edit--layout-signature)))

(defun nova-rime-tree--tree-p ()
  "整句视图：有完整读法，或者只差最后半个音节且上一次有树。单个音节也算。"
  (and (eq nova-rime-candidate-style 'tree) (not nova-rime--list-view) (nova-rime-edit--active-p)
       (let ((units (nova-rime-tree--units)))
         (or (and nova-rime--candidate-records
                  (= (nova-rime-candidate-end (car nova-rime--candidate-records)) (length units)))
             (and (nova-rime-tree--pending-p units) nova-rime-tree--blocks)))))

(defcustom nova-rime-tree-show-keys 'inline
  "输入时怎么显示原始按键，按音节用 ' 隔开，如 ta'uo've'ge。
`inline'：输入中的文字先显示成按键，汉字只出现在下面的词块里，空格确认后才显示汉字；
`above'：汉字照常显示，按键另起一行放在上方；nil：不显示按键。"
  :type '(choice (const inline) (const above) (const nil)) :group 'nova-rime)

(defface nova-rime-tree-keys-face
  '((t :underline t))
  "原始按键。" :group 'nova-rime)

(defun nova-rime-tree--unit-keys (units)
  "UNITS 每个单元要显示的按键：音节后面跟 '，下一个也是音节时才加；标点等文字原样。"
  (cl-loop for (unit next) on units
           collect (if (nova-rime-edit--unit-literal unit) (nova-rime-edit--unit-keys unit)
                     (concat (nova-rime-edit--unit-keys unit)
                             (and next (not (nova-rime-edit--unit-literal next)) "'")))))

(defun nova-rime-tree--show-keys-inline ()
  "把输入中的每个字显示成它的按键。字本身还在 buffer 里，光标照样逐个音节移动。"
  (cl-loop for unit in nova-rime-edit--units
           for keys in (nova-rime-tree--unit-keys nova-rime-edit--units)
           for p from (marker-position nova-rime-edit--start)
           unless (nova-rime-edit--unit-literal unit)
           do (let ((overlay (make-overlay p (1+ p))))
                (overlay-put overlay 'display (propertize keys 'face 'nova-rime-tree-keys-face))
                (push overlay nova-rime-edit--correction-decorations))))

(defun nova-rime-tree--show-keys-above ()
  "在这段文字所在行的上方插一行按键，左边和这段文字的开头对齐。"
  (let* ((window (car (get-buffer-window-list (current-buffer) nil t)))
         (column (car (cdr (nova-rime-edit--correction-location nova-rime-edit--start window))))
         ;; 折行时插在这段文字开头所在的视觉行前面，列号也是按视觉行算的。
         (line-start (save-excursion (goto-char nova-rime-edit--start)
                                     (if window (vertical-motion 0 window) (beginning-of-line))
                                     (point)))
         (overlay (make-overlay line-start line-start)))
    (overlay-put overlay 'before-string
                 (concat (nova-rime-tree--align column)
                         (propertize (apply #'concat (nova-rime-tree--unit-keys nova-rime-edit--units))
                                     'face 'nova-rime-tree-keys-face)
                         "\n"))
    (push overlay nova-rime-edit--correction-decorations)))

(defun nova-rime-tree--show-corrections (text)
  "半上屏的 `tree' 样式：画树；TAB 切到的整句列表仍用 nova-rime 自己的显示。
按键先显示，树的位置和折行要按显示出来的按键算。"
  (when (nova-rime-edit--active-p)
    (pcase nova-rime-tree-show-keys
      ((or 'inline 't) (nova-rime-tree--show-keys-inline))
      ('above (nova-rime-tree--show-keys-above))))
  (if (nova-rime-tree--tree-p)
      (nova-rime-tree--show text)
    (nova-rime-edit--show-corrections text)))

(defun nova-rime-tree--post-command ()
  "光标在句子里移动时，焦点跟着变，需要重画。"
  (when (and (bound-and-true-p nova-rime-edit-mode) (nova-rime-edit--active-p) nova-rime-tree--blocks
             (not (eql (nova-rime-tree--focus-index nova-rime-tree--blocks) nova-rime-tree--focus)))
    (nova-rime-edit--render)))


(provide 'nova-rime-tree)
;;; nova-rime-tree.el ends here
