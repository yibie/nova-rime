;;; wime-tree.el --- Tree candidates and sentence correction -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; `wime-candidate-style' 设为 `tree' 时启用；`wime-raw-input' 的原始字母输入也用它画树。
;;
;; 把当前读法切成词块，用连线串起来，备选挂在词块下面。和 Nova Squirrel 的改错方式一致：
;; - 不确定的词（橙、紫）直接列出备选，全句连续编号，按数字就改。
;; - 光标停在某个词里时只展开这个词，数字只选它；光标回到句尾恢复全句编号。
;; - 打过去的词冻结：不再被后文改掉。冻结只在搜索时作为软约束加进 `wime--pins'，
;;   不写进读音单元，所以 wime 的上屏学习看到的仍是它自己的首选。
;; - 最后一个音节没打完时，文字和树都保持上一次的样子。
;; 选字沿用 wime 的 `wime--span-choices' 与 `wime--choose-span'，这里只决定选哪些、怎么画。

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar wime--input)
(defvar wime--pins)
(defvar wime--candidates)
(defvar wime--candidate-records)
(defvar wime--span-choices)
(defvar wime--list-view)
(defvar wime-candidate-style)
(defvar wime-show-candidates)
(defvar wime-word-penalty)
(defvar wime-tree-show-keys)
(defvar wime-edit--start)
(defvar wime-edit--end)
(defvar wime-edit--units)
(defvar wime-edit--correction-decorations)
(defvar wime-edit--correction-layout)
(defvar wime-raw-input)
(declare-function wime--parse "wime")
(declare-function wime--span-code "wime")
(declare-function wime--unit-complete-p "wime")
(declare-function wime--entries-for "wime")
(declare-function wime--matching-entries "wime")
(declare-function wime-candidate-end "wime")
(declare-function wime-edit--active-p "wime-edit")
(declare-function wime-edit--render "wime-edit")
(declare-function wime-edit--syllables "wime-edit")
(declare-function wime-edit--unit-literal "wime-edit")
(declare-function wime-edit--unit-keys "wime-edit")
(declare-function wime-edit--unit-code "wime-edit")
(declare-function wime-edit--shown "wime-edit")
(declare-function wime-edit--offset "wime-edit")
(declare-function wime-edit--display-width "wime-edit")
(declare-function wime-edit--correction-location "wime-edit")
(declare-function wime-edit--layout-signature "wime-edit")
(declare-function wime-edit--show-corrections "wime-edit")

(defface wime-tree-confident-face
  '((t :background "#2f6fde" :foreground "white"))
  "有把握的词块。" :group 'wime)

(defface wime-tree-unsure-face
  '((t :background "#d26624" :foreground "white"))
  "没把握的词块，备选直接列出。" :group 'wime)

(defface wime-tree-corrected-face
  '((t :background "#7b48d8" :foreground "white"))
  "整句选了词频较低的词。" :group 'wime)

(defface wime-tree-pinned-face
  '((t :background "#1f9d6b" :foreground "white"))
  "用户选过的词。" :group 'wime)

(defface wime-tree-focus-face
  '((t :background "#f5c518" :foreground "black"))
  "光标所在的词。" :group 'wime)

(defface wime-tree-line-face
  '((t :strike-through "#8a8a8a"))
  "连接词块的线。" :group 'wime)

(defface wime-tree-branch-face
  '((t :foreground "#8a8a8a"))
  "备选前的分支符号。" :group 'wime)

(defface wime-tree-current-face
  '((t :background "#2f6fde" :foreground "white"))
  "上下键换词时，句子里正在用的那一项。" :group 'wime)

(defcustom wime-tree-max-block 3
  "词块最多几个字。词库有「我很喜欢」这样的短语，块太长就没有东西可选。"
  :type 'integer :group 'wime)
(defcustom wime-tree-confident-share 80
  "占比低于它算没把握（0–100）。调高会有更多词显示成橙色、直接列出备选。"
  :type 'integer :group 'wime)
(defcustom wime-tree-freeze t
  "非 nil 时，打过去的多字词有把握或已稳定就冻结，后文不再改它。
nil 时每打一个音节整句都可能重新解码（rime 原本的行为）。"
  :type 'boolean :group 'wime)
(defcustom wime-tree-freeze-share 50
  "多字词占比达到它，打过去就冻结。"
  :type 'integer :group 'wime)
(defcustom wime-tree-stable-syllables 2
  "多字词之后又打了这么多音节都没变，就冻结。"
  :type 'integer :group 'wime)
(defcustom wime-tree-overview 'uncertain
  "没有焦点时哪些词直接列出备选、能按数字改。
`uncertain'：只有没把握的词；`all'：每个词（编号用完为止）；nil：都不列，要先移光标到词上。"
  :type '(choice (const :tag "只列没把握的词" uncertain)
                 (const :tag "每个词都列" all)
                 (const :tag "不列，只在焦点词上列" nil))
  :group 'wime)
(defcustom wime-tree-overview-alts 2
  "没有焦点时每个词列几个备选。"
  :type 'integer :group 'wime)
(defcustom wime-tree-focus-alts 6
  "焦点词每页几个备选。"
  :type 'integer :group 'wime)
(defcustom wime-tree-focus-pages 6
  "焦点词的备选最多翻几页。"
  :type 'integer :group 'wime)
(defcustom wime-tree-char-alts 4
  "光标停在多字词的某个字上时，备选先列这个字的几个同音单字，后面才是整个词的写法。
选单字只换这一个字，词里其他字不动。设为 0 不列单字。"
  :type 'integer :group 'wime)
(defcustom wime-tree-same-length-alts 4
  "焦点词里同长度备选最多几个，其余位置留给别的长度（用来重新切分）。"
  :type 'integer :group 'wime)
(defcustom wime-tree-labels 9
  "全句最多几个编号。"
  :type 'integer :group 'wime)

(cl-defstruct (wime-tree-block (:constructor wime-tree--make-block))
  start end text keys share corrected pinned (age 0) alts)

(defvar-local wime-tree--blocks nil
  "上一次画出的词块。最后一个音节没打完时继续用它。")
(defvar-local wime-tree--frozen nil
  "冻结的词，(START END TEXT KEYS)。KEYS 是这几个音节的按键，对不上就作废。")
(defvar-local wime-tree--syllables 0
  "上一次的完整音节数，用来判断是不是多打了一个音节。")
(defvar-local wime-tree--focus nil
  "上一次画出时光标所在的词块序号，nil 表示没有焦点。")
(defvar-local wime-tree--focus-syllable nil
  "上一次画出时光标所在的音节序号。")
(defvar-local wime-tree--page 0 "焦点词的备选翻到第几页，从 0 开始。")
(defvar-local wime-tree--page-key nil "翻页对应的 (输入 . 焦点词起点)，变了就回到第一页。")
(defvar-local wime-tree--pages 1 "焦点词的备选一共几页。")
(defvar-local wime-tree--current nil "本页里句子正在用的那一项（上下键换词时），从 0 开始。")
(defvar-local wime-tree--cycle nil
  "上下键换词的状态，plist：:alts 第一次按键时记下的备选，:index 当前第几项（0 是原来的词），
:pins :frozen :blocks :syllables 第一次按键前的状态，每次换词都从它出发。")

;;; 数据

(defun wime-tree--units ()
  (wime--parse (or wime--input "")))

(defun wime-tree--complete-count (units)
  (cl-loop for unit in units while (wime--unit-complete-p unit) count t))

(defun wime-tree--keys (units start end)
  (mapcar #'car (seq-subseq units start end)))

(defun wime-tree--weights (units start length)
  "从 START 起 LENGTH 个音节的词，((WORD . WEIGHT) ...)，按词频从高到低，已按辅助码过滤。"
  (let ((end (+ start length)) seen)
    (when (<= end (length units))
      (dolist (entry (wime--matching-entries (wime--entries-for (wime--span-code units start end))
                                             (seq-subseq units start end)))
        (let ((word (car entry)) (weight (or (nth 2 entry) 0)))
          (when (and (= (length word) length) (not (assoc word seen)))
            (push (cons word weight) seen))))
      (sort (nreverse seen) (lambda (a b) (> (cdr a) (cdr b)))))))

(defun wime-tree--share (weights word)
  "WORD 占 WEIGHTS 词频的比例（0-100）；不在里面时为 nil。"
  (when-let* ((own (cdr (assoc word weights))))
    (let ((total (apply #'+ (mapcar #'cdr weights))))
      (when (> total 0) (round (* 100.0 own) total)))))

(defun wime-tree--word-ends (text units)
  "把 TEXT 切成词，返回每个词的结束位置。
和 wime 的词格同一套打分：各词 log 词频之和，每多一个词扣 `wime-word-penalty'。
只取最长词会切出「他说这」「很难解」这种词库里有、但不像人话的块。
词库里没有的单字按词频 1 算，保证总能切开。"
  (let* ((n (length text))
         (best (make-vector (1+ n) nil)))
    (aset best 0 (cons 0.0 nil))
    (dotimes (end n)
      (let ((end (1+ end)))
        (cl-loop for start from (max 0 (- end wime-tree-max-block)) below end
                 for previous = (aref best start)
                 when previous do
                 (let* ((word (substring text start end))
                        (weight (cdr (assoc word (wime-tree--weights units start (- end start))))))
                   (when (or weight (= (- end start) 1))
                     (let ((score (+ (car previous) (log (max 1 (or weight 1))) (- wime-word-penalty))))
                       (when (or (null (aref best end)) (> score (car (aref best end))))
                         (aset best end (cons score (cons end (cdr previous)))))))))))
    (reverse (cdr (aref best n)))))

(defun wime-tree--alternatives (block units limit same-length)
  "BLOCK 起点开始的其他写法：先放 SAME-LENGTH 个同长度的，再每种长度一个，最多 LIMIT 个。
每项是 (START END TEXT)，END 可以和块不同，用来重新切分。"
  (let* ((start (wime-tree-block-start block))
         (length (- (wime-tree-block-end block) start))
         (seen (list (wime-tree-block-text block)))
         result)
    (cl-flet ((add (len count)
                (cl-loop for (word . _) in (wime-tree--weights units start len)
                         while (and (> count 0) (< (length result) limit))
                         unless (member word seen)
                         do (push word seen) (push (list start (+ start len) word) result)
                         (cl-decf count))))
      (add length same-length)
      (cl-loop for other from (1- length) downto 1 do (add other 1))
      (cl-loop for other from (1+ length) to wime-tree-max-block do (add other 1))
      (add length limit))
    (nreverse result)))

(defun wime-tree--focus-list (block units syllable)
  "焦点词的全部备选。光标 SYLLABLE 在多字词里时，先放这个字的几个同音单字，
再放整个词的写法，剩下的单字排在后面。单字项带第四个元素 (START END TEXT)，
是它所在的词，选它时词里其他字原样冻结。"
  (let* ((limit (* wime-tree-focus-alts wime-tree-focus-pages))
         (start (wime-tree-block-start block))
         (end (wime-tree-block-end block))
         (text (wime-tree-block-text block))
         (whole (wime-tree--alternatives block units limit wime-tree-same-length-alts))
         (split (and syllable (> wime-tree-char-alts 0) (> (- end start) 1)
                     (<= start syllable) (< syllable end)
                     (list start end text)))
         (current (and split (char-to-string (aref text (- syllable start)))))
         (chars (and split
                     (cl-loop for (word . _) in (wime-tree--weights units syllable 1)
                              unless (equal word current)
                              collect (list syllable (1+ syllable) word split)))))
    (if (not chars)
        whole
      (let* ((first (min wime-tree-char-alts (length chars)))
             (words (max 0 (- wime-tree-focus-alts first))))
        (seq-take (append (seq-take chars first) (seq-take whole words)
                          (nthcdr first chars) (nthcdr words whole))
                  limit)))))

(defun wime-tree--build (text units)
  "把 TEXT 切成词块，算出比例、是否被纠正、是否用户选过、稳定了几个音节。"
  (let* ((pinned (mapcar #'car wime--pins))
         (complete (wime-tree--complete-count units))
         (grew (> complete wime-tree--syllables))
         (start 0) blocks)
    (dolist (end (wime-tree--word-ends text units))
      (let* ((word (substring text start end))
             (weights (wime-tree--weights units start (- end start)))
             (share (wime-tree--share weights word))
             (previous (seq-find (lambda (b) (and (= (wime-tree-block-start b) start)
                                                  (= (wime-tree-block-end b) end)
                                                  (equal (wime-tree-block-text b) word)))
                                 wime-tree--blocks)))
        ;; 一两个字的词不在词库里，多半是切错了，按 0 算。
        (when (and (not share) weights (< (- end start) 3)) (setq share 0))
        (push (wime-tree--make-block
               :start start :end end :text word
               :keys (wime-tree--keys units start end)
               :share share
               :corrected (and share (seq-some (lambda (w) (> (cdr w) (or (cdr (assoc word weights)) 0)))
                                               weights))
               :pinned (cl-loop for i from start below end always (memq i pinned))
               :age (if previous (+ (wime-tree-block-age previous) (if grew 1 0)) 0))
              blocks)
        (setq start end)))
    (setq wime-tree--syllables complete)
    (nreverse blocks)))

(defun wime-tree--uncertain-p (block)
  (and (not (wime-tree-block-pinned block))
       (or (wime-tree-block-corrected block)
           (and (wime-tree-block-share block)
                (< (wime-tree-block-share block) wime-tree-confident-share)))))

;;; 冻结

(defun wime-tree--freeze (block)
  (let ((start (wime-tree-block-start block)))
    (setq wime-tree--frozen
          (cons (list start (wime-tree-block-end block)
                      (wime-tree-block-text block) (wime-tree-block-keys block))
                (seq-remove (lambda (f) (= (car f) start)) wime-tree--frozen)))))

(defun wime-tree--update-frozen (blocks)
  "不是最后一块的多字词，有把握或已经稳定，就冻结。关掉 `wime-tree-freeze' 时不冻结。"
  (cl-loop for block in (and wime-tree-freeze (butlast blocks))
           when (and (>= (length (wime-tree-block-text block)) 2)
                     (or (>= (or (wime-tree-block-share block) 0) wime-tree-freeze-share)
                         (>= (wime-tree-block-age block) wime-tree-stable-syllables))
                     (not (seq-find (lambda (f) (= (car f) (wime-tree-block-start block)))
                                    wime-tree--frozen)))
           do (wime-tree--freeze block)))

(defun wime-tree--soft-pins (units)
  "仍然有效的冻结，按 `wime--pins' 的格式 (POSITION CHAR CODE)。用户选过的字优先。"
  (let ((hard (mapcar #'car wime--pins)))
    (cl-loop for (start end text keys) in wime-tree--frozen
             when (and (<= end (length units)) (equal keys (wime-tree--keys units start end)))
             append (cl-loop for i from start below end
                             unless (memq i hard)
                             collect (list i (aref text (- i start)) (car (nth i units)))))))

(defun wime-tree--search-pins (input)
  "`wime--candidates' 搜索 INPUT 时用的 `wime--pins'：整句搜索带上冻结的字。
别处按单个音节查词时不加。"
  (if (and wime-tree--frozen (equal input wime--input))
      (append wime--pins (wime-tree--soft-pins (wime--parse input)))
    wime--pins))

(defun wime-tree--freeze-span (start end text units)
  (when (< start end)
    (setq wime-tree--frozen
          (cons (list start end text (wime-tree--keys units start end))
                (seq-remove (lambda (f) (= (car f) start)) wime-tree--frozen)))))

(defun wime-tree--choose-span (start end &optional split)
  "选词时其余词块全部冻结，只重新解码和所选范围重叠的部分。
SPLIT 是 (START END TEXT)：只换了这个词里的一个字，词里其他字也冻结。"
  (setq wime-tree--frozen
        (seq-remove (lambda (f) (and (< (car f) end) (> (nth 1 f) start))) wime-tree--frozen))
  (dolist (block wime-tree--blocks)
    (when (or (<= (wime-tree-block-end block) start) (>= (wime-tree-block-start block) end))
      (wime-tree--freeze block)))
  (pcase split
    (`(,word-start ,word-end ,text)
     (let ((units (wime-tree--units)))
       (when (<= word-end (length units))
         (wime-tree--freeze-span word-start start (substring text 0 (- start word-start)) units)
         (wime-tree--freeze-span end word-end (substring text (- end word-start)) units))))))

(defun wime-tree--reset ()
  (setq wime-tree--blocks nil wime-tree--frozen nil
        wime-tree--syllables 0 wime-tree--focus nil wime-tree--focus-syllable nil
        wime-tree--page 0 wime-tree--page-key nil wime-tree--cycle nil))

;;; 上下键换词

(defun wime-tree--snapshot ()
  (list :pins wime--pins :frozen wime-tree--frozen
        :blocks wime-tree--blocks :syllables wime-tree--syllables))

(defun wime-tree--restore (state)
  (setq wime--pins (plist-get state :pins)
        wime-tree--frozen (plist-get state :frozen)
        wime-tree--blocks (plist-get state :blocks)
        wime-tree--syllables (plist-get state :syllables)))

(defun wime-tree--cycle-step (delta)
  "换到焦点词的下一项（DELTA 为 1）或上一项（-1）。
第一次按时记下备选和当前状态，之后每次都从这个状态出发，所以一路按回去能回到原来的词。
返回要套用的备选；回到原来的词时返回 t；到头了返回 nil。"
  (unless wime-tree--cycle
    (when-let* ((focus wime-tree--focus)
                (block (nth focus wime-tree--blocks))
                (alts (wime-tree--focus-list block (wime-tree--units) wime-tree--focus-syllable)))
      (setq wime-tree--cycle (append (list :alts alts :index 0) (wime-tree--snapshot)))))
  (when wime-tree--cycle
    (let* ((alts (plist-get wime-tree--cycle :alts))
           (index (+ (plist-get wime-tree--cycle :index) delta)))
      (when (and (>= index 0) (<= index (length alts)))
        (setq wime-tree--cycle (plist-put wime-tree--cycle :index index))
        (wime-tree--restore wime-tree--cycle)
        (if (= index 0) t (nth (1- index) alts))))))

(defun wime-tree--cycle-restore ()
  "上下键换词中按数字：先回到换词前的状态，再按编号选。"
  (when wime-tree--cycle
    (wime-tree--restore wime-tree--cycle)))

(defun wime-tree--turn-page (delta)
  "焦点词的备选翻页。翻不动时返回 nil。"
  (let ((page (+ wime-tree--page delta)))
    (when (and wime-tree--focus (>= page 0) (< page wime-tree--pages))
      (setq wime-tree--page page wime-tree--cycle nil)
      t)))

;;; 半个音节

(defun wime-tree--pending-p (units)
  "最后一个音节没打完，前面都是完整音节。
开着 `wime-abbreviate' 时单个声母也算完整音节（简拼），不会没打完。"
  (and units (not (wime--unit-complete-p (car (last units))))
       (= (wime-tree--complete-count units) (1- (length units)))))

(defun wime-tree--pending-text ()
  "最后一个音节没打完时，已经显示的字保持不动，返回这段文字；否则返回 nil。
wime 原本会逐字重新猜。"
  (let ((syllables (and (wime-edit--active-p) (wime-edit--syllables wime-edit--units))))
    (when (and syllables
             (eq wime-candidate-style 'tree)
             (wime-tree--pending-p (wime-tree--units))
             (cl-every #'wime-edit--shown (butlast syllables)))
        (concat (mapconcat #'wime-edit--shown (butlast syllables) "")
                (or (wime-edit--shown (car (last syllables)))
                    (substring (wime-edit--unit-keys (car (last syllables))) 0 1))))))

;;; 焦点与编号

(defun wime-tree--edit-focus-syllable ()
  "半上屏：光标前一个字是第几个音节。光标在句尾时没有焦点。"
  (when (and (wime-edit--active-p) (< (point) wime-edit--end) (>= (point) wime-edit--start))
    (max 0 (1- (cl-loop for unit in (seq-take wime-edit--units (- (point) wime-edit--start))
                        count (not (wime-edit--unit-literal unit)))))))

(defvar-local wime-tree--focus-syllable-function #'wime-tree--edit-focus-syllable
  "返回光标所在的音节序号，没有焦点时返回 nil。半上屏和原始字母输入各有一个。")

(defun wime-tree--focus-index (blocks)
  "光标所在音节所在的词块。"
  (when-let* ((syllable (setq wime-tree--focus-syllable
                              (funcall wime-tree--focus-syllable-function))))
    (cl-position-if (lambda (b) (and (<= (wime-tree-block-start b) syllable)
                                     (< syllable (wime-tree-block-end b))))
                    blocks)))

(defun wime-tree--assign (blocks units focus)
  "给要显示备选的块填上 alts，并按编号顺序设置 `wime--span-choices'。"
  (dolist (block blocks) (setf (wime-tree-block-alts block) nil))
  (setq wime-tree--pages 1 wime-tree--current nil)
  (if focus
      (let* ((block (nth focus blocks))
             (cycle wime-tree--cycle)
             (index (and cycle (plist-get cycle :index)))
             (all (if cycle
                      (plist-get cycle :alts)
                    (wime-tree--focus-list block units wime-tree--focus-syllable)))
             (per (max 1 wime-tree-focus-alts))
             (key (cons wime--input (wime-tree-block-start block))))
        (setq wime-tree--pages (max 1 (ceiling (length all) (float per))))
        (cond
         ;; 换词时翻到正在用的那一项所在的页。
         ((and cycle (> index 0))
          (setq wime-tree--page (/ (1- index) per)
                wime-tree--current (% (1- index) per)))
         (cycle)
         ((not (equal key wime-tree--page-key))
          (setq wime-tree--page 0)))
        (setq wime-tree--page-key key
              wime-tree--page (min wime-tree--page (1- wime-tree--pages)))
        (setf (wime-tree-block-alts block)
              (seq-take (nthcdr (* wime-tree--page per) all) per)))
    ;; 编号不够时最不确定的先拿，每块要么全有要么没有。
    (let ((labels 0))
      (dolist (block (sort (pcase wime-tree-overview
                             ('all (seq-remove #'wime-tree-block-pinned blocks))
                             ('uncertain (seq-filter #'wime-tree--uncertain-p blocks)))
                           (lambda (a b) (< (or (wime-tree-block-share a) 0)
                                            (or (wime-tree-block-share b) 0)))))
        (let ((alts (wime-tree--alternatives block units wime-tree-overview-alts
                                                  wime-tree-overview-alts)))
          (when (and alts (<= (+ labels (length alts)) wime-tree-labels))
            (setf (wime-tree-block-alts block) alts)
            (cl-incf labels (length alts)))))))
  (setq wime--span-choices (cl-loop for block in blocks append (wime-tree-block-alts block))))

;;; 显示

(defun wime-tree--face (block focused)
  (cond (focused 'wime-tree-focus-face)
        ((wime-tree-block-pinned block) 'wime-tree-pinned-face)
        ((wime-tree-block-corrected block) 'wime-tree-corrected-face)
        ((wime-tree--uncertain-p block) 'wime-tree-unsure-face)
        (t 'wime-tree-confident-face)))

(defun wime-tree--align (column &optional face)
  (propertize " " 'display `(space :align-to ,column) 'face face))

(defun wime-tree--chunk (columns)
  "COLUMNS 是 (CHIP-X CHIP CELLS-X CELLS)：一排词块和挂在下面的备选。"
  (let ((height (apply #'max 0 (mapcar (lambda (c) (length (nth 3 c))) columns))))
    (concat
     "\n"
     (cl-loop for (x chip) in columns
              concat (concat (wime-tree--align x 'wime-tree-line-face) chip))
     (mapconcat (lambda (row)
                  (concat "\n" (cl-loop for (_x _chip cells-x cells) in columns
                                        for cell = (nth row cells)
                                        when cell concat (concat (wime-tree--align cells-x) cell))))
                (number-sequence 0 (1- height)) ""))))

(defun wime-tree--rows (blocks focus left window)
  "从 LEFT 开始画词块。词块只按自身宽度排，备选放不下时往右让，不推动词块。"
  (let* ((limit (if window
                    (- (window-body-width window)
                       (with-selected-window window (line-number-display-width 'columns)) 1)
                  most-positive-fixnum))
         (gap (wime-edit--display-width "──" window))
         (branch (propertize "╰" 'face 'wime-tree-branch-face))
         (x (max left 0)) (floor 0) (label 0) chunks chunk)
    (cl-loop for block in blocks for i from 0 do
             (let* ((chip (propertize (concat " " (wime-tree-block-text block) " ")
                                      'face (wime-tree--face block (eql i focus))))
                    (width (wime-edit--display-width chip window))
                    (focused (eql i focus))
                    (cells (cl-loop for alt in (wime-tree-block-alts block) for j from 0
                                    collect
                                    (concat branch
                                            (propertize (number-to-string (% (cl-incf label) 10))
                                                        'face 'wime-label-face)
                                            (propertize (nth 2 alt) 'face
                                                        (if (and focused (eql j wime-tree--current))
                                                            'wime-tree-current-face
                                                          'wime-alternatives-face))))))
               (when (and focused cells (> wime-tree--pages 1))
                 (setcar (last cells)
                         (concat (car (last cells))
                                 (propertize (format " ‹ %d/%d ›" (1+ wime-tree--page) wime-tree--pages)
                                             'face 'wime-tree-branch-face))))
               (when (and chunk (> (+ x width) limit))
                 (push (nreverse chunk) chunks)
                 (setq chunk nil x 0 floor 0))
               (let ((cells-x (max x floor)))
                 (push (list x chip cells-x cells) chunk)
                 (when cells
                   (setq floor (+ cells-x gap (apply #'max (mapcar (lambda (c) (wime-edit--display-width c window))
                                                                   cells))))))
               (setq x (+ x width gap))))
    (when chunk (push (nreverse chunk) chunks))
    (mapconcat #'wime-tree--chunk (nreverse chunks) "")))

(defun wime-tree--draw (blocks focus start last)
  "在 START 到 LAST 这段文字下面画树，返回画出的 overlay。LAST 是最后一个字的位置。"
  (let ((windows (or (get-buffer-window-list (current-buffer) nil t) '(nil))) overlays)
    (dolist (window windows overlays)
      (pcase-let* ((`(,start-anchor ,start-column ,_) (wime-edit--correction-location start window))
                   (`(,anchor ,_ ,soft) (wime-edit--correction-location last window))
                   (left (if (eq start-anchor anchor) start-column 0))
                   (overlay (make-overlay anchor anchor)))
        (when window (overlay-put overlay 'window window))
        (overlay-put overlay 'after-string
                     (concat (wime-tree--rows blocks focus left window) (when soft "\n")))
        (push overlay overlays)))))

(defun wime-tree--present (text start last)
  "按 TEXT 重新切块，选出备选，画在 START 到 LAST 下面。返回画出的 overlay。
原始字母输入时最后半个音节不参与，TEXT 只覆盖前面的完整音节；
半上屏时沿用上一次的词块。"
  (let* ((all (wime-tree--units))
         (pending (wime-tree--pending-p all))
         (units (if pending (butlast all) all))
         (blocks (cond ((not pending) (wime-tree--build text units))
                       ((and wime-raw-input (= (length text) (length units)))
                        (wime-tree--build text units))
                       (t wime-tree--blocks)))
         (focus (wime-tree--focus-index blocks)))
    (unless pending (wime-tree--update-frozen blocks))
    (setq wime-tree--blocks blocks wime-tree--focus focus)
    (wime-tree--assign blocks units focus)
    (when wime-show-candidates
      (wime-tree--draw blocks focus start last))))

(defun wime-tree--show (text)
  "半上屏：画树，按键没替换文字时给有备选的词块加波浪线。"
  (let ((overlays (wime-tree--present
                   text wime-edit--start
                   (+ wime-edit--start (wime-edit--offset (1- (length text)))))))
    (dolist (block (unless (memq wime-tree-show-keys '(inline t)) wime-tree--blocks))
      (when (wime-tree-block-alts block)
        (let ((overlay (make-overlay (+ wime-edit--start (wime-edit--offset (wime-tree-block-start block)))
                                     (+ wime-edit--start 1 (wime-edit--offset (1- (wime-tree-block-end block)))))))
          (overlay-put overlay 'face 'wime-uncertain-face)
          (overlay-put overlay 'priority 1)
          (push overlay overlays))))
    (setq wime-edit--correction-decorations (append overlays wime-edit--correction-decorations)))
  (setq wime-edit--correction-layout (wime-edit--layout-signature)))

(defun wime-tree--tree-p ()
  "整句视图：有完整读法，或者只差最后半个音节且上一次有树。单个音节也算。"
  (and (eq wime-candidate-style 'tree) (not wime--list-view) (wime-edit--active-p)
       (let ((units (wime-tree--units)))
         (or (and wime--candidate-records
                  (= (wime-candidate-end (car wime--candidate-records)) (length units)))
             (and (wime-tree--pending-p units) wime-tree--blocks)))))

(defcustom wime-tree-show-keys 'inline
  "输入时怎么显示原始按键，按音节用 ' 隔开，如 ta'uo've'ge。
`inline'：输入中的文字先显示成按键，汉字只出现在下面的词块里，空格确认后才显示汉字；
`above'：汉字照常显示，按键另起一行放在上方；nil：不显示按键。"
  :type '(choice (const inline) (const above) (const nil)) :group 'wime)

(defface wime-tree-keys-face
  '((t :underline t))
  "原始按键。" :group 'wime)

(defun wime-tree--unit-keys (units)
  "UNITS 每个单元要显示的按键：音节后面跟 '，下一个也是音节时才加；标点等文字原样。"
  (cl-loop for (unit next) on units
           collect (if (wime-edit--unit-literal unit) (wime-edit--unit-keys unit)
                     (concat (wime-edit--unit-keys unit)
                             (and next (not (wime-edit--unit-literal next)) "'")))))

(defun wime-tree--show-keys-inline ()
  "把输入中的每个字显示成它的按键。字本身还在 buffer 里，光标照样逐个音节移动。"
  (cl-loop for unit in wime-edit--units
           for keys in (wime-tree--unit-keys wime-edit--units)
           for p from (marker-position wime-edit--start)
           unless (wime-edit--unit-literal unit)
           do (let ((overlay (make-overlay p (1+ p))))
                (overlay-put overlay 'display (propertize keys 'face 'wime-tree-keys-face))
                (push overlay wime-edit--correction-decorations))))

(defun wime-tree--show-keys-above ()
  "在这段文字所在行的上方插一行按键，左边和这段文字的开头对齐。"
  (let* ((window (car (get-buffer-window-list (current-buffer) nil t)))
         (column (car (cdr (wime-edit--correction-location wime-edit--start window))))
         ;; 折行时插在这段文字开头所在的视觉行前面，列号也是按视觉行算的。
         (line-start (save-excursion (goto-char wime-edit--start)
                                     (if window (vertical-motion 0 window) (beginning-of-line))
                                     (point)))
         (overlay (make-overlay line-start line-start)))
    (overlay-put overlay 'before-string
                 (concat (wime-tree--align column)
                         (propertize (apply #'concat (wime-tree--unit-keys wime-edit--units))
                                     'face 'wime-tree-keys-face)
                         "\n"))
    (push overlay wime-edit--correction-decorations)))

(defun wime-tree--show-corrections (text)
  "半上屏的 `tree' 样式：画树；TAB 切到的整句列表仍用 wime 自己的显示。
按键先显示，树的位置和折行要按显示出来的按键算。"
  (when (wime-edit--active-p)
    (pcase wime-tree-show-keys
      ((or 'inline 't) (wime-tree--show-keys-inline))
      ('above (wime-tree--show-keys-above))))
  (if (wime-tree--tree-p)
      (wime-tree--show text)
    (wime-edit--show-corrections text)))

(defun wime-tree--post-command ()
  "光标在句子里移动时，焦点跟着变，需要重画。"
  (when (and (bound-and-true-p wime-edit-mode) (wime-edit--active-p) wime-tree--blocks
             (not (eql (wime-tree--focus-index wime-tree--blocks) wime-tree--focus)))
    (wime-edit--render)))


(provide 'wime-tree)
;;; wime-tree.el ends here
