;;; nova-rime-test.el --- Nova-rime input method engine -*- lexical-binding: t; -*-

;;; Commentary:
;; The engine is pure Elisp, so these tests exercise the whole input path
;; without a module, a subprocess, or a graphical frame.

;;; Code:

(require 'ert)
(require 'nova-rime)

(ert-deftest nova-rime-context-uses-existing-text-at-point-not-only-ime-commits ()
  (with-temp-buffer
    (insert (propertize (concat (make-string 70 ?前) "这个函数") 'face 'bold) "后面的文字")
    (backward-char 5)
    (let ((nova-rime--input "fhhv") (position (point)) (tick (buffer-chars-modified-tick)))
      (should (equal (nova-rime--context) (concat (make-string 60 ?前) "这个函数")))
      (should-not (text-properties-at 0 (nova-rime--context)))
      (should (= position (point)))
      (should (= tick (buffer-chars-modified-tick))))))

(ert-deftest nova-rime-context-preserves-paragraphs-unless-explicitly-bounded ()
  (dolist (separator '("\n\n" "\n \t\n" "\f"))
    (with-temp-buffer
      (insert "上一段无关内容" separator "这个函数。\n返回")
      (should (equal (nova-rime--context) (buffer-string)))
      (let ((nova-rime-context-paragraph-only t))
        (should (equal (nova-rime--context) "这个函数。\n返回"))
        (insert "\n\n")
        (should (equal (nova-rime--context) ""))))))

(ert-deftest nova-rime-context-respects-narrowing-and-field-edges ()
  (with-temp-buffer
    (insert "历史当前输入")
    (narrow-to-region 3 (point-max))
    (should (equal (nova-rime--context) "当前输入"))
    (widen)
    (add-text-properties 1 3 '(field history rear-nonsticky t))
    (let ((inhibit-field-text-motion t))
      (should (equal (nova-rime--context) "当前输入"))
      (goto-char 3)
      (should (equal (nova-rime--context) "")))))

(ert-deftest nova-rime-context-stops-at-read-only-text-and-overlays ()
  (dolist (overlay '(nil t))
    (with-temp-buffer
      (insert "旧内容|当前输入")
      (if overlay
          (overlay-put (make-overlay 4 5) 'read-only t)
        (put-text-property 4 5 'read-only t))
      (should (equal (nova-rime--context) "当前输入"))
      (let ((buffer-read-only t) (inhibit-read-only t))
        (should (equal (nova-rime--context) "")))))
  (with-temp-buffer
    (insert (propertize "提示>" 'read-only t 'rear-nonsticky t))
    (should (equal (nova-rime--context) ""))
    (insert "输入")
    (should (equal (nova-rime--context) "输入"))
    (goto-char 2)
    (should (equal (nova-rime--context) ""))))

(ert-deftest nova-rime-context-does-not-join-across-hidden-text ()
  (dolist (overlay '(nil t))
    (with-temp-buffer
      (insert "之前隐藏当前")
      (if overlay
          (overlay-put (make-overlay 3 5 nil t) 'invisible t)
        (put-text-property 3 5 'invisible t))
      (let ((buffer-invisibility-spec t))
        (should (equal (nova-rime--context) "当前"))
        ;; Insertion before a non-front-sticky hidden span is still visible.
        (save-excursion
          (goto-char 3)
          (should (equal (nova-rime--context) "之前"))))
      (let ((buffer-invisibility-spec nil))
        (should (equal (nova-rime--context) "之前隐藏当前"))))))

(ert-deftest nova-rime-context-excludes-even-an-unprotected-minibuffer-prompt ()
  (with-temp-buffer
    (insert "查找文字：当前输入")
    (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 6)))
      (should (equal (nova-rime--context) "当前输入"))
      (goto-char 6)
      (should (equal (nova-rime--context) "")))))

(ert-deftest nova-rime-context-honors-current-comint-input-without-text-properties ()
  (require 'comint)
  (with-temp-buffer
    (comint-mode)
    (insert "历史输出\n提示> 输入\n第二行")
    (let ((process (make-pipe-process :name "nova-rime-context-comint-test"
                                      :buffer (current-buffer) :noquery t)))
      (unwind-protect
          (progn
            (set-marker (process-mark process) 10)
            (should (equal (nova-rime--context) "输入\n第二行"))
            (goto-char 5)
            (should (equal (nova-rime--context) "")))
        (delete-process process)))))

(ert-deftest nova-rime-context-honors-eshell-input-and-a-buffer-local-provider ()
  (with-temp-buffer
    (insert "历史输出 当前输入")
    (setq-local major-mode 'eshell-mode
                eshell-last-output-end (copy-marker 6))
    (should (equal (nova-rime--context) "当前输入"))
    (setq-local nova-rime-context-start-function (lambda () 8))
    (should (equal (nova-rime--context) "输入"))
    (setq-local nova-rime-context-start-function (lambda () (goto-char 8) (point)))
    (should (equal (nova-rime--context) "输入"))
    (should (= (point) (point-max)))
    ;; A provider can narrow but cannot reopen the shell's history.
    (setq-local nova-rime-context-start-function (lambda () 1))
    (should (equal (nova-rime--context) "当前输入"))
    (setq-local nova-rime-context-start-function (lambda () (error "unavailable")))
    (should (equal (nova-rime--context) ""))
    (setq-local nova-rime-context-start-function (lambda () (make-marker)))
    (should (equal (nova-rime--context) ""))))

(defmacro nova-rime-test--entries (entries &rest body)
  "Run BODY with a small in-memory dictionary ENTRIES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'nova-rime--entries-for)
              (lambda (code) (cdr (assoc code ,entries))))
             ((symbol-function 'nova-rime--grammar-score)
              (lambda (&rest _) 0.0)))
     ,@body))

(defmacro nova-rime-test--memory (entries &rest body)
  "Run BODY with dictionary ENTRIES and an empty user database file."
  (declare (indent 1))
  `(let* ((nova-rime-user-database (make-temp-file "nova-rime-user" nil ".sqlite3")) (nova-rime-learn t)
          (nova-rime--user-db nil) (nova-rime--memory (make-hash-table :test #'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'nova-rime--database-entries)
                    (lambda (code) (cdr (assoc code ,entries))))
                   ((symbol-function 'nova-rime--grammar-score) (lambda (&rest _) 0.0)))
           ,@body)
       (when nova-rime--user-db (sqlite-close (cdr nova-rime--user-db)))
       (delete-file nova-rime-user-database))))

(defun nova-rime-test--best (units)
  "The lattice's best reading of UNITS."
  (nova-rime-candidate-text (car (nova-rime--sentence-records units))))

(defconst nova-rime-test--homophones
  '(("dl" ("代" "a" 100) ("待" "b" 50)) ("xr" ("选" "d" 100)) ("xd" ("项" "e" 100))
    ("dlxr" ("代选" "a/d" 30) ("待选" "b/d" 20)) ("xrxd" ("选项" "d/e" 1000))))

(ert-deftest nova-rime-learning-is-off-unless-enabled ()
  (nova-rime-test--memory nova-rime-test--homophones
    (let ((units (nova-rime--parse "dlxrxd")) (nova-rime-learn nil))
      (nova-rime--learn "待选项" units "代选项")
      (should (zerop (hash-table-count nova-rime--memory)))
      (should-not (nova-rime--memory-entries "dlxr"))
      (should (equal (nova-rime-test--best units) "代选项")))))

(ert-deftest nova-rime-correction-learns-the-covering-word-until-contradicted ()
  (nova-rime-test--memory nova-rime-test--homophones
    (let ((units (nova-rime--parse "dlxrxd")) (tick nova-rime--memory-tick))
      (should (equal (nova-rime-test--best units) "代选项"))
      (nova-rime--learn "待选项" units "代选项")
      ;; One differing character becomes the dictionary word, never a lone 待.
      (should (equal (gethash "dlxr" nova-rime--memory) '(("待选" . "b/d"))))
      (should-not (gethash "dl" nova-rime--memory))
      (should (> nova-rime--memory-tick tick))
      (should (equal (nova-rime-test--best units) "待选项"))
      ;; Persisted across sessions.
      (sqlite-close (cdr nova-rime--user-db))
      (setq nova-rime--user-db nil)
      (clrhash nova-rime--memory)
      (should (equal (nova-rime-test--best units) "待选项"))
      ;; Committing another reading for the same keys replaces the preference.
      (nova-rime--learn "代选项" units "待选项")
      (should (equal (gethash "dlxr" nova-rime--memory) '(("代选" . "a/d"))))
      (should (equal (sqlite-select (nova-rime--user-db) "SELECT code, text FROM memory")
                     '(("dlxr" "代选")))))))

(ert-deftest nova-rime-span-pin-correction-is-learned-against-the-unpinned-offer ()
  (nova-rime-test--memory nova-rime-test--homophones
    (with-temp-buffer
      (setq nova-rime--input "dlxrxd")
      (nova-rime--refresh)
      (nova-rime--choose-span 0 1 "待")
      (should (equal (car nova-rime--candidates) "待选项"))
      (should (equal nova-rime--default-reading "代选项"))
      (nova-rime-input-method ?\s)
      (should (equal (gethash "dlxr" nova-rime--memory) '(("待选" . "b/d")))))))

(ert-deftest nova-rime-correction-learns-the-frequent-word-not-one-across-a-boundary ()
  (nova-rime-test--memory '(("ru" ("入" "a" 100)) ("ui" ("是" "b" 100) ("时" "c" 90)) ("hb" ("候" "d" 100))
                       ("ruui" ("入时" "a/c" 50)) ("uihb" ("时候" "c/d" 9000)))
    (let ((units (nova-rime--parse "ruuihb")))
      (nova-rime--learn "入时候" units "入是候")
      (should (equal (gethash "uihb" nova-rime--memory) '(("时候" . "c/d"))))
      (should-not (gethash "ruui" nova-rime--memory)))))

(ert-deftest nova-rime-commit-logs-the-offer-and-learns-only-corrections ()
  (nova-rime-test--memory nova-rime-test--homophones
    (with-temp-buffer
      (setq-local nova-rime-candidate-style 'list)
      (insert "按数字键选择")
      (setq nova-rime--input "dlxrxd")
      (nova-rime--refresh)
      (should (equal nova-rime--default-reading "代选项"))
      (nova-rime--commit "代选项")
      (should (zerop (hash-table-count nova-rime--memory)))
      (setq nova-rime--input "dlxrxd")
      (nova-rime--refresh)
      (setq nova-rime--candidate-records
            (append nova-rime--candidate-records
                    (list (nova-rime--make-candidate :text "待选项" :end 3 :score -1)))
            nova-rime--candidates (mapcar #'nova-rime-candidate-text nova-rime--candidate-records))
      (nova-rime--select-digit (+ ?1 (seq-position nova-rime--candidates "待选项")))
      (should (equal (sqlite-select (nova-rime--user-db)
                                    "SELECT input, keys, text, default_reading, context FROM commits")
                     '(("dlxrxd" "dlxrxd" "代选项" "代选项" "按数字键选择")
                       ("dlxrxd" "dlxrxd" "待选项" "代选项" "按数字键选择代选项"))))
      (should (equal (gethash "dlxr" nova-rime--memory) '(("待选" . "b/d")))))))

(ert-deftest nova-rime-search-cache-follows-editable-context-and-boundaries ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 1))
                        ("bb" ("丙" "c" 100)))
    (cl-letf (((symbol-function 'nova-rime--grammar-score)
               (lambda (context word _rear)
                 (if (and (equal context "前") (equal word "乙")) 20.0 0.0))))
      (with-temp-buffer
        (let ((units (nova-rime--parse "aabb")))
          (should (equal (car (nova-rime--candidates "aabb")) "甲丙"))
          (insert "前")
          (should (equal (car (nova-rime--candidates "aabb")) "乙丙"))
          (should (equal (nova-rime--sentence-records units)
                         (let ((nova-rime--search-cache nil)) (nova-rime--sentence-records units)))))
        ;; The text and point do not change, only what belongs to this input.
        (setq-local nova-rime-context-start-function #'point)
        (should (equal (car (nova-rime--candidates "aabb")) "甲丙"))))))

(ert-deftest nova-rime-single-syllable-keeps-display-slots-and-auxiliary-constraints ()
  (with-temp-buffer
    (let ((nova-rime-search-width 1) (nova-rime-candidates-max 10))
      (should (= (length (nova-rime--candidates "qd")) 10))
      (should (cl-every (lambda (c) (and (= (nova-rime-candidate-end c) 1)
                                        (nova-rime-candidate-dictionary c)))
                       nova-rime--candidate-records))
      (insert "真刀真")
      (should (equal (car (nova-rime--candidates "qdM")) "枪"))
      (should (equal (car (nova-rime--candidates "qdG")) "强"))
      (should-not (member "枪" (nova-rime--candidates "qdG")))
      (nova-rime--candidates "q")
      (should-not nova-rime--candidate-records))))

(ert-deftest nova-rime-prefix-search-recovers-pruned-reading-without-changing-cache ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 1))
                        ("bb" ("丙" "c" 100)))
    (let* ((nova-rime-search-width 1) (nova-rime--search-cache nil)
           (units (nova-rime--parse "aabb"))
           (original (nova-rime--sentence-records units)) (cache nova-rime--search-cache))
      (should (equal (nova-rime-candidate-text (car original)) "甲丙"))
      (should (equal (nova-rime-candidate-text (car (nova-rime--sentence-records units "乙"))) "乙丙"))
      (should (eq cache nova-rime--search-cache))
      (should-not (seq-some (lambda (c) (= (nova-rime-candidate-end c) 2))
                            (nova-rime--sentence-records (nova-rime--parse "aaAbb") "乙"))))))

(ert-deftest nova-rime-prefix-search-handles-word-interior-and-impossible-prefix ()
  (nova-rime-test--entries '(("aabb" ("甲乙" "a/b" 100) ("丙丁" "c/d" 1)))
    (let ((units (nova-rime--parse "aabb")) (nova-rime-search-width 1))
      (should (equal (nova-rime-candidate-text (car (nova-rime--sentence-records units "丙"))) "丙丁"))
      (should-not (nova-rime--sentence-records units "甲乙丙"))
      (should-not (nova-rime--sentence-records units "甲丁")))))

;; Character positions disagree where readings compete; marks follow the
;; posterior and alternatives come from every scored reading.
(ert-deftest nova-rime-beam-keeps-alternative-full-readings ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 90))
                        ("bb" ("丙" "c" 100) ("丁" "d" 90)))
    (let* ((nova-rime-search-width 4)
           (records (nova-rime--sentence-records (nova-rime--parse "aabb")))
           (full (seq-filter (lambda (c) (= (nova-rime-candidate-end c) 2)) records)))
      (should (= (length full) 4))
      (should (member "乙丁" (mapcar #'nova-rime-candidate-text full)))
      (should (seq-some (lambda (c) (= (nova-rime-candidate-end c) 1)) records))
      (should-not (seq-some #'nova-rime-candidate-dictionary full)))))

(ert-deftest nova-rime-beam-respects-auxiliary-codes-and-budget ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 90))
                        ("bb" ("丙" "c" 100) ("丁" "d" 90)))
    (let ((nova-rime-search-width 1))
      (should (equal (nova-rime-test--best (nova-rime--parse "aaBbbD"))
                     "乙丁"))
      (should (= 1 (length (seq-filter
                            (lambda (c) (= (nova-rime-candidate-end c) 2))
                            (nova-rime--sentence-records (nova-rime--parse "aabb")))))))))

(ert-deftest nova-rime-beam-retains-prefix-until-later-context-disambiguates ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 90))
                        ("bb" ("丙" "c" 100)))
    (cl-letf (((symbol-function 'nova-rime--grammar-score)
               (lambda (context _word _rear) (if (equal context "乙") 10.0 0.0))))
      (let ((nova-rime-search-width 2))
        (should (equal (nova-rime-test--best (nova-rime--parse "aabb"))
                       "乙丙"))))))

(ert-deftest nova-rime-beam-deduplicates-and-retains-dictionary-provenance ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100)) ("bb" ("乙" "b" 100))
                        ("aabb" ("甲乙" "a/b" 1)))
    ;; Even when composition beats the dictionary path, retain exact-hit evidence.
    (let* ((nova-rime-word-penalty 0)
           (records (nova-rime--sentence-records (nova-rime--parse "aabb")))
           (full (seq-filter (lambda (c) (= (nova-rime-candidate-end c) 2)) records)))
      (should (= (length full) 1))
      (should (nova-rime-candidate-dictionary (car full))))))

(ert-deftest nova-rime-beam-does-not-label-partial-reading-as-full ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100)))
    (let ((records (nova-rime--sentence-records (nova-rime--parse "aabb"))))
      (should records)
      (should (cl-every (lambda (c) (= (nova-rime-candidate-end c) 1)) records)))))

(ert-deftest nova-rime-incremental-search-matches-fresh-on-append-delete-and-aux ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100) ("乙" "b" 90))
                        ("bb" ("丙" "c" 100) ("丁" "d" 90))
                        ("cc" ("戊" "e" 100)) ("aabb" ("甲丙" "a/c" 50)))
    (let ((nova-rime--search-cache nil) (nova-rime-search-width 4))
      (cl-letf (((symbol-function 'nova-rime--grammar-score)
                 (lambda (_context word rear)
                   (if (and rear (equal word "丁")) 15.0 0.0))))
        (dolist (input '("aabb" "aabbcc" "aabbcc" "aabbDcc" "aabbD" "aaBbbD" "aabb"))
          (let* ((units (nova-rime--parse input))
                 (incremental (nova-rime--sentence-records units))
                 (fresh (let ((nova-rime--search-cache nil)) (nova-rime--sentence-records units))))
            (should (equal incremental fresh))))))))

(ert-deftest nova-rime-search-cache-invalidates-on-scoring-settings ()
  (nova-rime-test--entries '(("aa" ("甲" "a" 100)) ("bb" ("乙" "b" 100))
                        ("aabb" ("甲乙" "a/b" 1)))
    (let ((nova-rime--search-cache nil))
      (nova-rime--sentence-records (nova-rime--parse "aabb"))
      (let* ((nova-rime-word-penalty 0)
             (cached (nova-rime--sentence-records (nova-rime--parse "aabb")))
             (fresh (let ((nova-rime--search-cache nil))
                      (nova-rime--sentence-records (nova-rime--parse "aabb")))))
        (should (equal cached fresh))))))

;; An explicit test database or the monorepo build can exercise the released
;; word list.  A standalone checkout instead uses disposable fixture data;
;; never consult the installed word list or personal input history.
(setq nova-rime-database-file
      (let ((requested (getenv "NOVA_RIME_TEST_DATABASE"))
            (candidate (expand-file-name "bazel-bin/emacs-packages/wime/wanxiang.db")))
        (cond (requested
               (unless (file-readable-p requested)
                 (error "NOVA_RIME_TEST_DATABASE is not readable: %s" requested))
               (expand-file-name requested))
              ((file-readable-p candidate) candidate)
              (t
               (load (expand-file-name "fixture.el"
                                       (file-name-directory (or load-file-name buffer-file-name))) nil t)
               (nova-rime-test--fixture)))))

(defmacro nova-rime-test--type (keys &rest body)
  "Type KEYS into a temporary buffer and evaluate BODY."
  `(with-temp-buffer
     (nova-rime--reset)
     (mapc #'nova-rime-input-method (string-to-list ,keys))
     ,@body))

(ert-deftest nova-rime-uppercase-without-code-passes-through ()
  (with-temp-buffer
    (nova-rime--reset)
    (should (equal (nova-rime-input-method ?A) (list ?A)))))

(ert-deftest nova-rime-english-mode-commits-floating-composition-first ()
  (nova-rime-test--type "ni"
    (let ((selected (nth nova-rime--selected nova-rime--candidates)))
      (nova-rime-english-mode 1)
      (should (equal (nova-rime-input-method ?x) (list ?x)))
      (should (equal (buffer-string) selected))
      (should-not (nova-rime--active-p)))))

(ert-deftest nova-rime-parse-keeps-typed-order ()
  (should (equal (nova-rime--parse "yrZt") '(("yr" . "Z") ("t" . ""))))
  (should (equal (nova-rime--parse "nihk") '(("ni" . "") ("hk" . ""))))
  (should (equal (nova-rime--parse "niR") '(("ni" . "R"))))
  (should (equal (nova-rime--parse "y;") '(("y;" . "")))))

(ert-deftest nova-rime-lowercase-after-aux-starts-next-syllable ()
  (nova-rime-test--type "niKh"
    (should (equal nova-rime--input "niKh"))
    (should (equal (nova-rime--parse nova-rime--input) '(("ni" . "K") ("h" . ""))))))

(ert-deftest nova-rime-space-commits-the-selected-candidate ()
  (nova-rime-test--type "niK"
    (should (equal nova-rime--input "niK"))
    (should (equal (car nova-rime--candidates) "匿"))
    (nova-rime-input-method ?\s)
    (should (equal (buffer-string) "匿"))))

(ert-deftest nova-rime-digit-selects-a-candidate ()
  (nova-rime-test--type "hk"
    (nova-rime-input-method ?2)
    (should (equal (buffer-string) "号"))))

(ert-deftest nova-rime-backspace-removes-aux-then-code ()
  (nova-rime-test--type "niK"
    (nova-rime-delete-backward)
    (should (equal nova-rime--input "ni"))
    (nova-rime-delete-backward)
    (should (equal nova-rime--input "n"))))

(ert-deftest nova-rime-return-commits-raw-input ()
  (nova-rime-test--type "niK"
    (nova-rime-commit-raw)
    (should (equal (buffer-string) "niK"))))

(ert-deftest nova-rime-escape-cancels-the-composition ()
  (nova-rime-test--type "niK"
    (nova-rime-cancel)
    (should-not nova-rime--input)
    (should-not (nova-rime--active-p))))

(ert-deftest nova-rime-read-only-buffer-passes-through ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (nova-rime--reset)
    (should (equal (nova-rime-input-method ?n) (list ?n)))
    (should-not (nova-rime--active-p))))

(ert-deftest nova-rime-word-auxiliary-belongs-to-its-syllable ()
  (should (member "你好" (nova-rime--candidates "niRhk")))
  (should-not (member "你好" (nova-rime--candidates "niXhk"))))

(ert-deftest nova-rime-space-commits-a-word ()
  (nova-rime-test--type "nihk"
    (nova-rime-input-method ?\s)
    (should (equal (buffer-string) "你好"))))

(ert-deftest nova-rime-sentence-composition ()
  (should (equal (car (nova-rime--candidates "womfm;tm")) "我们明天"))
  (should (equal (car (nova-rime--candidates "xmzl")) "现在")))

(ert-deftest nova-rime-single-key-completion ()
  (should (member "你" (nova-rime--candidates "n")))
  (should (member "就" (nova-rime--candidates "j")))
  ;; A lone incomplete syllable completes to single characters only.
  (should (cl-every (lambda (word) (= (length word) 1))
                    (nova-rime--candidates "n")))
  ;; A complete prefix allows longer words.
  (should (member "你好" (nova-rime--candidates "nih"))))

(ert-deftest nova-rime-full-width-punctuation ()
  (with-temp-buffer
    (nova-rime--reset)
    (nova-rime-input-method ?\,)
    (nova-rime-input-method ?.)
    (should (equal (buffer-string) "，。")))
  (with-temp-buffer
    (nova-rime--reset)
    (let ((nova-rime-ascii-punctuation t))
      (should (equal (nova-rime-input-method ?\,) (list 44))))))

(defmacro nova-rime-test--with-database (file &rest body)
  "Evaluate BODY with FILE beside the test database, or skip."
  (declare (indent 1))
  `(let ((path (if (equal ,file "wanxiang.db") nova-rime-database-file
                 (expand-file-name ,file (file-name-directory nova-rime-database-file))))
         (saved nova-rime-database-file))
     (unless (file-readable-p path)
       (ert-skip (concat ,file " not built")))
     (unwind-protect
         (progn (nova-rime-select-database path) ,@body)
       (nova-rime-select-database saved))))

(ert-deftest nova-rime-scheme-keys-come-from-the-database ()
  (nova-rime-test--with-database "wanxiang.db"
    ;; 微软双拼 only uses ; for the final ing.
    (should-not (nova-rime--code-char-p ?\; 'initial))
    (should (nova-rime--code-char-p ?\; 'final))
    (should-not (nova-rime--code-char-p ?\, 'final))))

(ert-deftest nova-rime-tones-belong-to-their-syllable-and-are-not-code ()
  (should (equal (nova-rime--tones "nihk3") '((1 . 3))))
  (should (equal (nova-rime--tones "ni2hkK4") '((0 . 2) (1 . 4))))
  (should (equal (nova-rime--parse "ni2hkK4") (nova-rime--parse "nihkK")))
  (should (equal (nova-rime--retone "nihk3" 2) "nihk2"))
  (should (equal (nova-rime--retone "ni2hK" 4) "ni2hK4")))

(ert-deftest nova-rime-tone-key-retones-the-last-syllable-and-shows-its-mark ()
  (nova-rime-test--type "nihk"
    (cl-letf (((symbol-function 'nova-rime--refresh) #'ignore))
      (let ((last-command-event ?\C-3)) (nova-rime-tone))
      (let ((last-command-event ?\C-4)) (nova-rime-tone)))
    (should (equal nova-rime--input "nihk4"))
    (should (string-prefix-p "nihkˋ" (substring-no-properties (nova-rime--format))))))

(ert-deftest nova-rime-typed-tones-filter-the-lattice-by-character-readings ()
  (with-temp-buffer
    (let ((toneless (nova-rime--candidates "woyk")) (yao3 (nova-rime--candidates "woyk3")))
      (should (equal (car toneless) "我要"))
      (should-not (member "我要" yao3))
      (should (cl-every (lambda (text) (seq-contains-p (nova-rime--char-tones (aref text 1) "yk") ?3))
                        (seq-filter (lambda (text) (= (length text) 2)) yao3))))
    ;; Words record sandhi (一个 yí gè); the typed dictionary tone still reads them.
    (should (member "一个" (nova-rime--candidates "yi1ge4")))
    (should (equal (nova-rime--candidates "vozi5") (nova-rime--candidates "vozi")))
    ;; No jiu character is read in tone 2: nothing, not the unfiltered list.
    (should-not (nova-rime--candidates "jq2"))))

(provide 'nova-rime-test)
;;; nova-rime-test.el ends here
