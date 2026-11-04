;;; nova-rime.el --- Double-pinyin input method with tree candidates -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Version: 0.2.0
;; URL: https://github.com/yibie/nova-rime
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; A Rime-free double-pinyin input method using the Wanxiang word list and
;; uppercase Moqi auxiliary codes.  The scheme comes with the database: 微软双拼 by
;; default, or a table-generated one (see schemes/).
;;
;; The typed string is parsed into syllables: two code keys (letters, plus
;; whichever of ``;,./'' the scheme uses) optionally followed by uppercase
;; auxiliary keys.  The auxiliary code belongs to the syllable it follows, so
;; a lowercase key after it starts the next syllable.  This keeps the typed
;; order intact and never has to guess where an auxiliary code ends.
;;
;; Candidate lookup is plain Elisp over the generated single-character and
;; word tables, so the whole input path is testable in batch.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sqlite)
(require 'nova-rime-tree)
(require 'nova-rime-raw)

(declare-function nova-rime-grammar-open "librime-grammar"
                  (shared-data-dir language))
(declare-function nova-rime-grammar-query "librime-grammar" (context word rear))

(defgroup nova-rime nil
  "Nova-rime double-pinyin input method."
  :group 'leim)


(defface nova-rime-preedit-face
  '((t (:inherit font-lock-string-face)))
  "Face for the typed double-pinyin code."
  :group 'nova-rime)

(defface nova-rime-aux-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the typed auxiliary code."
  :group 'nova-rime)

(defface nova-rime-candidate-face
  '((t (:inherit default)))
  "Face for an unselected candidate."
  :group 'nova-rime)

(defface nova-rime-selected-face
  '((t (:inherit highlight)))
  "Face for the selected candidate."
  :group 'nova-rime)

(defface nova-rime-uncertain-face
  '((t (:underline (:style wave :color "orange"))))
  "Face for characters whose reading is uncertain."
  :group 'nova-rime)

(defface nova-rime-pinned-face
  '((t (:underline t)))
  "Face for characters fixed by span selection."
  :group 'nova-rime)

(defface nova-rime-alternatives-face
  '((t (:inherit nova-rime-candidate-face)))
  "Face for the text of a numbered span alternative."
  :group 'nova-rime)

(defface nova-rime-label-face
  '((t (:inherit shadow)))
  "Face for the digit before an inline span alternative."
  :group 'nova-rime)

(defface nova-rime-indicator-face
  '((t (:inherit (mode-line-emphasis font-lock-keyword-face) :weight bold)))
  "Face for the nova-rime indicator while it handles input."
  :group 'nova-rime)

(defface nova-rime-indicator-dim-face
  '((t (:inherit (mode-line shadow))))
  "Face for the nova-rime indicator while input passes through."
  :group 'nova-rime)

(defvar nova-rime-title "nova-rime"
  "The title shown in the mode line.")

(defvar nova-rime-english-title "英"
  "The title shown while `nova-rime-english-mode' inserts keys literally.")

(defcustom nova-rime-candidates-max 5
  "Maximum candidates in the sentence list or each uncertain span.
Numbered span alternatives share the ten digit keys across all spans."
  :type 'natnum
  :group 'nova-rime)

(defcustom nova-rime-data-directory
  (expand-file-name "nova-rime" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory holding the word database, models and worker environment."
  :type 'directory
  :group 'nova-rime)

(defcustom nova-rime-database-file (expand-file-name "wanxiang.db" nova-rime-data-directory)
  "SQLite database holding the full wanxiang word list, or nil.

gen/nova_rime_gen.py builds it; wdoom points this at its Bazel artifact.
A readable database and Emacs SQLite support are required for candidates."
  :type '(choice (const nil) file)
  :group 'nova-rime)

(defcustom nova-rime-database-limit 64
  "Maximum entries fetched from the database for one code."
  :type 'natnum
  :group 'nova-rime)

(defcustom nova-rime-abbreviate nil
  "Non-nil lets any syllable be typed as its initial alone, anywhere.
A key may then stand alone as an initial or pair with the next key into
a full syllable, so one input splits several ways: \"wxhn\" is either
\"wx\" \"hn\" or four initials.  `nova-rime--parse' picks the split whose
words score best, charging `nova-rime-abbreviation-penalty' per initial.
When nil, keys always pair up and only a trailing key stays incomplete.
Half-commit editing (`nova-rime-edit-mode') places characters by key pairs,
so it keeps pairing; use this with `nova-rime-raw-input'."
  :type 'boolean
  :group 'nova-rime)

(defcustom nova-rime-abbreviation-penalty 4.0
  "Score charged per syllable typed as an initial alone.
An initial matches every syllable it starts, so its best word usually
outweighs the exact reading; the charge keeps full syllables preferred
whenever they spell real words."
  :type 'number
  :group 'nova-rime)

(defcustom nova-rime-abbreviation-max-syllables 4
  "Longest word, in syllables, considered when splitting abbreviated input."
  :type 'natnum
  :group 'nova-rime)

(defcustom nova-rime-word-penalty 20.0
  "Penalty subtracted per word when scoring a sentence path."
  :type 'number
  :group 'nova-rime)

(defcustom nova-rime-max-word-syllables 6
  "Longest dictionary word, in syllables, that the lattice considers."
  :type 'natnum
  :group 'nova-rime)

(defcustom nova-rime-search-width 8
  "Maximum distinct paths retained at each syllable boundary."
  :type '(integer :tag "Paths (at least 1)")
  :group 'nova-rime)

(defcustom nova-rime-context-paragraph-only nil
  "When non-nil, stop context at a blank line or form feed.
By default paragraph separators are preserved within the editable input
area and 64-character limit: an earlier paragraph can help disambiguation.
Modes can set this buffer-locally when paragraphs are independent inputs."
  :type 'boolean :group 'nova-rime)

(defvar-local nova-rime-context-start-function nil
  "Optional function returning the start of this buffer's editable input.
Called without arguments at the insertion point.  Return a position or a
marker in this buffer, or nil for no additional boundary.  Modes with custom
editors can set this in their mode hook.  It can only narrow the context;
field, prompt, visibility, read-only and length limits still apply.
Return point to disable context.  Errors or invalid markers also yield none.")

(defvar-local nova-rime-context-prefix-function nil
  "Optional function returning text that precedes this buffer's input, or nil.
Called without arguments, for example by a chat buffer to supply its latest
messages before the composer.  The commit log records it with the buffer
context; the lattice never sees it.  Errors yield no prefix.")

(defun nova-rime--context-prefix ()
  "Return `nova-rime-context-prefix-function' text, or the empty string."
  (or (and nova-rime-context-prefix-function
           (condition-case nil
               (save-excursion (funcall nova-rime-context-prefix-function))
             (error nil)))
      ""))

(defvar eshell-last-output-end)

(defun nova-rime--input-start ()
  "Return the current input area's lower bound, clamped to accessible text."
  (let ((start (minibuffer-prompt-end))
        (mode-start
         (cond ((derived-mode-p 'comint-mode)
                (when-let* ((process (get-buffer-process (current-buffer))))
                  (process-mark process)))
               ((derived-mode-p 'eshell-mode)
                (and (boundp 'eshell-last-output-end) eshell-last-output-end))))
        (custom-start
         (when nova-rime-context-start-function
           (condition-case nil
               (save-excursion
                 (save-restriction
                   (funcall nova-rime-context-start-function)))
             (error (point))))))
    (dolist (bound (list mode-start custom-start))
      (cond ((null bound))
            ((integerp bound) (setq start (max start bound)))
            ((and (markerp bound) (eq (marker-buffer bound) (current-buffer)))
             (setq start (max start (marker-position bound))))
            (t (setq start (point)))))
    (max (point-min) (min (point) start))))

(defvar nova-rime-edit--start)
(declare-function nova-rime-edit--active-p "nova-rime-edit")
(declare-function nova-rime-edit--type-tone "nova-rime-edit" (tone))
(declare-function nova-rime-edit--type-literal "nova-rime-edit" (key))

(defun nova-rime--editing-p ()
  "Whether an editable run of `nova-rime-edit-mode' holds the composition."
  (and (bound-and-true-p nova-rime-edit-mode) (nova-rime-edit--active-p)))

(defun nova-rime--context (&optional limit)
  "Return committed context before the current composition."
  (save-excursion
    (when (nova-rime--editing-p)
      (goto-char nova-rime-edit--start))
    (nova-rime--context-at-point limit)))

(defun nova-rime--context-at-point (&optional limit)
  "Return the visible, editable suffix before the insertion point.
Honor narrowing, input-area and field boundaries; stop at protected or
hidden characters instead of joining text across them.  Paragraph limits
are optional; punctuation and single newlines are retained.
Keep at most LIMIT characters, 64 by default: the lattice and the syllable
model read no more."
  (save-excursion
    (save-match-data
      (if (or buffer-read-only (get-pos-property (point) 'read-only)
              (invisible-p (get-pos-property (point) 'invisible)))
          ""
        (let* ((end (point))
               (start (max (- end (or limit 64)) (nova-rime--input-start)))
               (start (max start (field-beginning end nil start)))
               (position end))
          ;; At most LIMIT characters, including overlay properties.  A read-only
          ;; prompt can abut an editable insertion point via rear-nonsticky.
          (while (and (> position start)
                      (not (get-char-property (1- position) 'read-only))
                      (not (invisible-p (1- position))))
            (cl-decf position))
          (setq start position)
          (goto-char end)
          (when (and nova-rime-context-paragraph-only
                     (re-search-backward "\n[ \t]*\n\\|\f" start t))
            (setq start (match-end 0)))
          (buffer-substring-no-properties start end))))))

(cl-defstruct (nova-rime-candidate (:constructor nova-rime--make-candidate))
  "A reading and the input it consumes; END counts parsed syllables."
  text score end dictionary)

(defvar-local nova-rime--candidate-records nil
  "Candidates with scores, input coverage and dictionary provenance.")

(defvar-local nova-rime--search-cache nil
  "Last complete search, as (SIGNATURE UNITS PATHS).")

(defvar-local nova-rime--pins nil
  "Characters fixed by span selection, as (POSITION CHAR CODE) lists.
Every search keeps them; a pin is dropped once the keys at POSITION change.")

(defvar-local nova-rime--typed-tones nil
  "Typed tones of the input being searched, as `nova-rime--tones' returns them.")

(defvar nova-rime--tone-cache nil
  "(DATABASE . TABLE) of single-character tone digits by (CODE . CHAR).")

(defun nova-rime--char-tones (char code)
  "Tone digits CHAR has when read as CODE, from the database."
  (let ((db (nova-rime--database)))
    (unless (eq (car nova-rime--tone-cache) db)
      (setq nova-rime--tone-cache (cons db (make-hash-table :test #'equal))))
    (with-memoization (gethash (cons code char) (cdr nova-rime--tone-cache))
      (mapconcat #'car (sqlite-select db "SELECT tones FROM entry WHERE code = ? AND word = ?"
                                      (list code (string char)))
                 ""))))

(defun nova-rime--tones-allow-p (word start units)
  "Whether WORD, read from syllable START of UNITS, has every typed tone it covers.
Character readings decide, so a typed 一 yi1 still reads 一个; the neutral
tone filters nothing."
  (cl-every (lambda (tone)
              (let ((offset (- (car tone) start)))
                (or (< offset 0) (>= offset (length word)) (= (cdr tone) 5)
                    (seq-contains-p (nova-rime--char-tones (aref word offset) (car (nth (car tone) units)))
                                    (+ ?0 (cdr tone))))))
            nova-rime--typed-tones))

(defun nova-rime--pins-allow-p (word start)
  "Whether WORD, read from syllable START, keeps every pin it covers."
  (cl-every (lambda (pin)
              (let ((offset (- (car pin) start)))
                (or (< offset 0) (>= offset (length word))
                    (eq (aref word offset) (nth 1 pin)))))
            nova-rime--pins))

(defvar nova-rime-candidates-updated-hook nil
  "Hook run after refreshing the traditional candidates.")

(defvar nova-rime-reset-hook nil
  "Hook run when a composition is discarded or committed.")

(defvar nova-rime-lattice-inhibit-functions nil
  "Functions of the parsed units; if one returns non-nil, skip the lattice.")

(defvar nova-rime-settle-hook nil
  "Hook run before a key commits or chooses from the candidates.")

(defcustom nova-rime-grammar-module nil
  "Path to the librime-grammar module, or nil to disable the language model."
  :type '(choice (const nil) file)
  :group 'nova-rime)

(defcustom nova-rime-grammar-shared-dir nil
  "Directory holding the octagram ``.gram'' model."
  :type '(choice (const nil) directory)
  :group 'nova-rime)

(defcustom nova-rime-grammar-language "wanxiang-lts-zh-hans"
  "Octagram language name."
  :type 'string
  :group 'nova-rime)

(defcustom nova-rime-ascii-punctuation nil
  "When non-nil, insert ASCII punctuation instead of full-width."
  :type 'boolean
  :group 'nova-rime)

(defcustom nova-rime-punctuation
  '((?\, . "，")
    (?\\ . "、")
    (?. . "。")
    (?\? . "？")
    (?\! . "！")
    (?\: . "：")
    (?\x28 . "（")
    (?\x29 . "）")
    (?\[ . "【")
    (?\] . "】")
    (?\{ . "『")
    (?\} . "』")
    (?< . "《")
    (?> . "》")
    (?^ . "……")
    (?_ . "——"))
  "Full-width replacements for ASCII punctuation keys."
  :type '(alist :key-type character :value-type string)
  :group 'nova-rime)

(defcustom nova-rime-show-candidates t
  "Whether to show the candidate list in the preedit."
  :type 'boolean
  :group 'nova-rime)

(defcustom nova-rime-candidate-style 'list
  "How a complete multi-syllable composition shows its candidates.
`list' numbers whole sentences.
`tree' splits the best reading into word chips joined by a line.  Words
the dictionary is unsure of list alternatives with sentence-wide labels;
with point inside a word only that word lists them.  Words already typed
past stay fixed while later syllables arrive.
\\<nova-rime-active-mode-map>\\[nova-rime-toggle-candidate-list] switches one
composition back to the sentence list."
  :type '(choice (const list) (const tree)) :group 'nova-rime)

(defvar-local nova-rime--input nil
  "Raw typed string, preserving the case and order of the keys.")

(defvar-local nova-rime--candidates nil
  "Candidates matching the current input.")

(defvar-local nova-rime--selected 0
  "Index into `nova-rime--candidates' of the selected candidate.")

(defvar-local nova-rime--span-choices nil
  "Displayed span alternatives in label order, as (START END TEXT) lists.")

(defvar-local nova-rime--list-view nil
  "Non-nil shows the sentence list despite `nova-rime-candidate-style'.")

(defvar-local nova-rime--overlay nil
  "Overlay showing the preedit and candidates.")

(defcustom nova-rime-editable-preedit nil
  "Display Chinese compositions as editable buffer text.
Enable `nova-rime-edit-mode' when activating nova-rime.  Readings survive cursor
movement and switching to literal input until explicitly confirmed."
  :type 'boolean :group 'nova-rime)

(autoload 'nova-rime-edit-mode "nova-rime-edit" nil t)
(defvar nova-rime-edit--keymap)
(defvar nova-rime-edit--generation)
(declare-function nova-rime-edit--render "nova-rime-edit")
(declare-function nova-rime-edit--decodable-p "nova-rime-edit")
(declare-function nova-rime-edit--pending-refresh "nova-rime-edit")
(declare-function nova-rime-edit--input-method "nova-rime-edit" (key))
(declare-function nova-rime-edit-delete-backward "nova-rime-edit")

(defconst nova-rime--default-keys "abcdefghijklmnopqrstuvwxyz;"
  "Code keys assumed when the database does not describe its scheme.")

(defvar nova-rime--position-keys nil
  "Cons (INITIAL-KEYS . FINAL-KEYS) read from the open database.")

(defun nova-rime--position-keys ()
  "Return (INITIAL-KEYS . FINAL-KEYS): the keys a syllable starts and ends with."
  (or nova-rime--position-keys
      (when-let* ((db (nova-rime--database))
                  (rows (ignore-errors
                          (sqlite-select db "SELECT key, value FROM meta"))))
        (setq nova-rime--position-keys
              (cons (cadr (assoc "initial_keys" rows))
                    (cadr (assoc "final_keys" rows)))))
      (cons nova-rime--default-keys nova-rime--default-keys)))

(defun nova-rime--code-char-p (key &optional position)
  "Return non-nil if KEY is a double-pinyin code character.

Letters always are.  A punctuation key such as ``;'' or ``,'' is a code
key only where the scheme uses it: POSITION is `initial' or `final', and
nil accepts either.  Elsewhere it stays punctuation."
  (and (integerp key)
       (or (<= ?a key ?z)
           (let ((keys (nova-rime--position-keys)))
             (string-search (char-to-string key)
                            (pcase position
                              ('initial (car keys))
                              ('final (cdr keys))
                              (_ (concat (car keys) (cdr keys)))))))))

(defun nova-rime--aux-char-p (key)
  "Return non-nil if KEY is an uppercase auxiliary code character."
  (and (integerp key) (<= ?A key ?Z)))

(defun nova-rime--digit-p (key)
  "Return non-nil if KEY is a digit."
  (and (integerp key) (<= ?0 key ?9)))

(defun nova-rime--digit-index (key)
  "The candidate index digit KEY selects: 1 is the first, 0 the tenth."
  (if (= key ?0) 9 (- key ?1)))

(defconst nova-rime--tone-marks "ˉˊˇˋ·"
  "Marks shown for typed tones 1 to 4 and the neutral tone 5.")

(defun nova-rime--tone-key-p (key)
  "Return non-nil if KEY is a typed tone `nova-rime-tone'.
`nova-rime--parse' skips it."
  (and (integerp key) (<= ?1 key ?5)))

(defun nova-rime--tones (input)
  "Typed tones of INPUT as (SYLLABLE . TONE) pairs, in syllable order."
  (let ((index -1) (code 2) tones)
    (dolist (key (string-to-list (or input "")) (nreverse tones))
      (cond ((nova-rime--code-char-p key)
             (when (= code 2) (setq index (1+ index) code 0))
             (setq code (1+ code)))
            ((and (nova-rime--tone-key-p key) (>= index 0))
             (setq tones (cons (cons index (- key ?0)) (assq-delete-all index tones))))))))

(defun nova-rime--retone (input tone)
  "INPUT with TONE typed for its last syllable in place of an earlier one."
  (let ((start 0) (code 2))
    (dotimes (i (length input))
      (when (nova-rime--code-char-p (aref input i))
        (when (= code 2) (setq start i code 0))
        (setq code (1+ code))))
    (concat (substring input 0 start)
            (seq-remove #'nova-rime--tone-key-p (substring input start))
            (number-to-string tone))))

(defun nova-rime-tone ()
  "Type the tone of the syllable just typed: C-1 to C-4, C-5 for the neutral tone.
Tones 1 to 4 keep only characters with that reading; all reach the model."
  (interactive)
  (let ((tone (- (event-basic-type last-command-event) ?0)))
    (if (nova-rime--editing-p)
        (nova-rime-edit--type-tone tone)
      (when (nova-rime--parse nova-rime--input)
        (setq nova-rime--input (nova-rime--retone nova-rime--input tone))
        (nova-rime--refresh)))))

(defun nova-rime--parse (input)
  "Split INPUT into a list of (CODE . AUX) syllables.
With `nova-rime-abbreviate' the split comes from `nova-rime--abbreviated-parse'."
  (or (and (nova-rime--abbreviating-p) (nova-rime--abbreviated-parse input))
      (nova-rime--paired-parse input)))

(defun nova-rime--paired-parse (input)
  "Split INPUT into a list of (CODE . AUX) syllables.

CODE holds one or two lowercase keys (``;'' is a key); a lowercase key
after a complete syllable starts a new syllable.  AUX collects the
uppercase keys typed after that syllable, so the typed order is kept."
  (let (units code aux)
    (dolist (key (string-to-list (or input "")))
      (cond
       ((nova-rime--code-char-p key)
        (when (= (length code) 2)
          (push (cons code (or aux "")) units)
          (setq code nil aux nil))
        (setq code (concat code (char-to-string key))))
       ((nova-rime--aux-char-p key)
        (setq aux (concat aux (char-to-string key))))))
    (when (or code aux)
      (push (cons (or code "") (or aux "")) units))
    (nreverse units)))

(defvar nova-rime--abbreviation-cache (make-hash-table :test #'equal)
  "Lookups made by `nova-rime--abbreviated-parse': a GLOB pattern to its
entries, and (PATTERN) to whether any code starts with it.")

(defvar nova-rime--abbreviation-cache-key nil
  "Database and limit the cache was filled from; a change clears it.")

(defvar nova-rime--abbreviation-state nil
  "(KEYS . BEST) of the last `nova-rime--abbreviated-parse', so typing one more
key only scores the splits ending at it.")

(defun nova-rime--abbreviation-cached (key compute)
  (let ((source (list nova-rime-database-file nova-rime-database-limit)))
    (unless (equal source nova-rime--abbreviation-cache-key)
      (clrhash nova-rime--abbreviation-cache)
      (setq nova-rime--abbreviation-cache-key source nova-rime--abbreviation-state nil)))
  (let ((hit (gethash key nova-rime--abbreviation-cache 'none)))
    (if (eq hit 'none) (puthash key (funcall compute) nova-rime--abbreviation-cache) hit)))

(defun nova-rime--abbreviation-entries (pattern)
  (nova-rime--abbreviation-cached
   pattern (lambda () (if (string-search "?" pattern)
                          (nova-rime--pattern-entries pattern)
                        (nova-rime--database-entries pattern)))))

(defun nova-rime--abbreviation-alive-p (pattern)
  "Non-nil when some dictionary code starts with PATTERN."
  (nova-rime--abbreviation-cached
   (list pattern)
   (lambda () (nova-rime--pattern-alive-p pattern))))

(defun nova-rime--abbreviation-splits (keys start end)
  "Ways to split KEYS from START to END into at most
`nova-rime-abbreviation-max-syllables' syllables of one or two keys, as
lists of (CODE . AUX).  A split stops growing once no word starts with it."
  (let ((limit (min nova-rime-abbreviation-max-syllables nova-rime-max-word-syllables)) result)
    (cl-labels ((grow (at units pattern count)
                  (if (= at end)
                      (push (reverse units) result)
                    (when (< count limit)
                      (dolist (size '(1 2))
                        (when (<= (+ at size) end)
                          (let* ((part (seq-subseq keys at (+ at size)))
                                 (code (mapconcat #'car part ""))
                                 (next (concat pattern code (if (= size 1) "?" ""))))
                            (when (nova-rime--abbreviation-alive-p next)
                              (grow (+ at size)
                                    (cons (cons code (mapconcat #'cdr part "")) units)
                                    next (1+ count))))))))))
      (grow start nil "" 0))
    result))

(defun nova-rime--abbreviated-parse (input)
  "Split INPUT's keys into syllables, each two keys or an initial alone.
Every key may start a syllable; a word scores its log weight less
`nova-rime-word-penalty', and `nova-rime-abbreviation-penalty' per
initial alone.
The best-scoring split wins, so full syllables hold unless initials
spell a more likely sentence.  Return nil without a database or when
no split reaches the end."
  (when (and input (nova-rime--database))
    (let* ((keys (let (result)
                   ;; (KEY . AUX): uppercase keys attach to the key before them.
                   (dolist (key (string-to-list input) (nreverse result))
                     (cond ((nova-rime--code-char-p key) (push (cons (char-to-string key) "") result))
                           ((and (nova-rime--aux-char-p key) result)
                            (setcdr (car result) (concat (cdar result) (char-to-string key))))))))
           (n (length keys))
           (old (car nova-rime--abbreviation-state))
           (old-best (cdr nova-rime--abbreviation-state))
           ;; best[END] depends only on the keys before END.
           (common (cl-loop for a in old for b in keys while (equal a b) count t))
           (best (make-vector (1+ n) nil)))
      (aset best 0 (cons 0.0 nil))
      (cl-loop for end from 1 to (min common n) do (aset best end (aref old-best end)))
      (cl-loop for end from (1+ common) to n do
               (cl-loop for start from (max 0 (- end (* 2 nova-rime-abbreviation-max-syllables)))
                        below end
                        for previous = (aref best start)
                        when previous do
                        (dolist (units (nova-rime--abbreviation-splits keys start end))
                          (let* ((pattern (mapconcat (lambda (unit)
                                                       (if (= (length (car unit)) 1)
                                                           (concat (car unit) "?")
                                                         (car unit)))
                                                     units ""))
                                 (entry (car (nova-rime--matching-entries
                                              (nova-rime--abbreviation-entries pattern) units))))
                            (when entry
                              (let ((score (+ (car previous)
                                              (log (max 1 (or (nth 2 entry) 1)))
                                              (- nova-rime-word-penalty)
                                              (- (* nova-rime-abbreviation-penalty
                                                    (cl-count-if (lambda (unit) (= (length (car unit)) 1))
                                                                 units))))))
                                (when (or (null (aref best end)) (> score (car (aref best end))))
                                  (aset best end (cons score (append (reverse units) (cdr previous)))))))))))
      (setq nova-rime--abbreviation-state (cons keys best))
      (when-let* ((found (aref best n)))
        (reverse (cdr found))))))

(defun nova-rime--next-position ()
  "Return `final' when the next code key completes a syllable, else `initial'."
  (let ((last (car (last (nova-rime--parse nova-rime--input)))))
    (if (and last (= (length (car last)) 1) (string-empty-p (cdr last)))
        'final
      'initial)))

(defun nova-rime--full-code (units)
  "Return the concatenated double-pinyin code of UNITS."
  (mapconcat #'car units ""))

(defun nova-rime--syllable-match-p (entry-aux typed)
  "Return non-nil if TYPED matches one comma-separated group of ENTRY-AUX."
  (or (string-empty-p typed)
      (seq-some (lambda (group) (string-prefix-p (downcase typed) group))
                (split-string entry-aux "," t))))

(defun nova-rime--entry-match-p (entry-aux units)
  "Return non-nil if ENTRY-AUX matches the auxiliary codes in UNITS.

Word entries separate syllables with ``/''; single-character entries are
a single syllable.  A syllable matches when the typed auxiliary code is a
prefix of one of its comma-separated groups."
  (let ((syllables (split-string entry-aux "/")))
    (and (= (length syllables) (length units))
         (cl-every (lambda (unit syllable)
                     (nova-rime--syllable-match-p syllable (cdr unit)))
                   units syllables))))

(defvar nova-rime--db nil
  "SQLite connection to `nova-rime-database-file'.")

(defvar nova-rime--db-failed nil
  "Non-nil once opening the database has failed.")

(defun nova-rime--database ()
  "Return the SQLite connection, or nil when unavailable."
  (when (and nova-rime-database-file (file-readable-p nova-rime-database-file))
    (unless (or nova-rime--db nova-rime--db-failed)
      (condition-case err
          (setq nova-rime--db (sqlite-open nova-rime-database-file))
        (error
         (setq nova-rime--db-failed t)
         (message "nova-rime: cannot open %s: %s" nova-rime-database-file err))))
    nova-rime--db))

(defun nova-rime-select-database (file)
  "Switch to the word database FILE, and with it to its double-pinyin scheme."
  (interactive
   (list (read-file-name "nova-rime database: "
                         (and nova-rime-database-file (file-name-directory nova-rime-database-file))
                         nova-rime-database-file t)))
  (when nova-rime--db
    (ignore-errors (sqlite-close nova-rime--db)))
  (setq nova-rime-database-file (and file (expand-file-name file))
        nova-rime--db nil
        nova-rime--db-failed nil
        nova-rime--position-keys nil)
  (nova-rime--reset))

(defun nova-rime--database-entries (code)
  "Return the word entries for CODE from the database."
  (when-let* ((db (nova-rime--database)))
    (sqlite-select db (concat "SELECT word, aux, weight FROM entry WHERE code = ? "
                              "ORDER BY weight DESC LIMIT ?")
                   (list code nova-rime-database-limit))))

(defvar nova-rime--grammar-ready nil)

(defvar nova-rime--grammar-failed nil)

(defun nova-rime--grammar-ready-p ()
  "Load the grammar module once and return non-nil when it is usable."
  (when (and nova-rime-grammar-module nova-rime-grammar-shared-dir
             (not nova-rime--grammar-ready) (not nova-rime--grammar-failed))
    (condition-case err
        (progn
          (module-load (expand-file-name nova-rime-grammar-module))
          (setq nova-rime--grammar-ready
                (and (fboundp 'nova-rime-grammar-open)
                     (nova-rime-grammar-open (expand-file-name nova-rime-grammar-shared-dir)
                                        nova-rime-grammar-language))))
      (error
       (setq nova-rime--grammar-failed t)
       (message "nova-rime: grammar module unavailable: %s" err))))
  nova-rime--grammar-ready)

(defun nova-rime--grammar-score (context word rear)
  "Return the octagram collocation score for WORD after CONTEXT."
  (if (nova-rime--grammar-ready-p)
      (nova-rime-grammar-query (or context "") word rear)
    0.0))

(defcustom nova-rime-user-database nil
  "SQLite file of this user's commit log and learned readings, or nil.
Each committed complete reading records its raw input, keys, text, the
reading nova-rime offered first and the context before it.  With
`nova-rime-learn', corrections also become learned readings kept here."
  :type '(choice (const nil) file) :group 'nova-rime)

(defcustom nova-rime-learn nil
  "Learn readings from corrections in `nova-rime-user-database'.
Where committed text differs from the reading nova-rime offered first, the
differing span, widened to the shortest dictionary word under the same keys,
becomes a learned reading that the lattice and the decoder strongly prefer.
Committing another reading for those keys forgets it.  Off by default: a
learned reading also wins where the context wants another word."
  :type 'boolean :group 'nova-rime)

(defcustom nova-rime-user-weight 1e7
  "Dictionary weight of a learned reading, above any wanxiang entry."
  :type 'number :group 'nova-rime)

(defvar nova-rime--user-db nil
  "Open `nova-rime-user-database', as (FILE . CONNECTION).")

(defvar nova-rime--memory (make-hash-table :test #'equal)
  "Learned readings: code -> list of (TEXT . AUX).")

(defvar nova-rime--memory-tick 0
  "Incremented whenever learned readings change; part of the search cache key.")

(defvar-local nova-rime--default-reading nil
  "First candidate offered for this input, before any pin or selection.")

(defun nova-rime--reload-memory (db)
  "Reload learned readings from DB and invalidate earlier search results."
  (let ((rows (sqlite-select db "SELECT code, text, aux FROM memory")))
    (clrhash nova-rime--memory)
    (pcase-dolist (`(,code ,text ,aux) rows)
      (push (cons text aux) (gethash code nova-rime--memory)))
    (cl-incf nova-rime--memory-tick)))

(defun nova-rime--user-db ()
  "Return the user database connection, loading learned readings once."
  (when nova-rime-user-database
    (unless (equal (car nova-rime--user-db) nova-rime-user-database)
      (make-directory (file-name-directory (expand-file-name nova-rime-user-database)) t)
      (let ((db (sqlite-open nova-rime-user-database)))
        (sqlite-execute db (concat "CREATE TABLE IF NOT EXISTS commits (time REAL, input TEXT, keys TEXT, "
                                   "text TEXT, default_reading TEXT, context TEXT)"))
        (sqlite-execute db (concat "CREATE TABLE IF NOT EXISTS memory (code TEXT, text TEXT, aux TEXT, "
                                   "count INTEGER, updated REAL, PRIMARY KEY (code, text))"))
        (setq nova-rime--user-db (cons nova-rime-user-database db))
        (nova-rime--reload-memory db)))
    (cdr nova-rime--user-db)))

(defun nova-rime--learned-db ()
  "The user database when `nova-rime-learn' applies its learned readings."
  (and nova-rime-learn (nova-rime--user-db)))

(defun nova-rime--memory-entries (code)
  "Learned readings for CODE, as database entries."
  (when (nova-rime--learned-db)
    (mapcar (lambda (m) (list (car m) (cdr m) nova-rime-user-weight)) (gethash code nova-rime--memory))))

(defun nova-rime--memory-spans (units)
  "Learned readings within UNITS' leading complete syllables, as (START . TEXT)."
  (let ((n (cl-loop for unit in units while (= (length (car unit)) 2) count t)) spans)
    (when (and (> n 1) (nova-rime--learned-db) (> (hash-table-count nova-rime--memory) 0))
      (dotimes (s n)
        (cl-loop for e from (+ s 2) to (min n (+ s 16)) do
                 (dolist (m (gethash (nova-rime--span-code units s e) nova-rime--memory))
                   (push (cons s (car m)) spans)))))
    (nreverse spans)))

(defun nova-rime--dictionary-entry (word units)
  "WORD's dictionary entry under the keys of UNITS, or nil."
  (seq-find (lambda (entry) (equal (car entry) word))
            (nova-rime--database-entries (nova-rime--span-code units 0 (length units)))))

(defun nova-rime--learned-aux (word units)
  "Auxiliary codes of WORD read as UNITS, from the dictionary."
  (or (nth 1 (nova-rime--dictionary-entry word units))
      (mapconcat (lambda (i)
                   (or (nth 1 (nova-rime--dictionary-entry (string (aref word i)) (list (nth i units)))) ""))
                 (number-sequence 0 (1- (length word))) "/")))

(defun nova-rime--learned-span (text units start end)
  "Shortest dictionary word of TEXT covering START..END under UNITS' keys.
Among equally short words the most frequent wins, so 输入时候 learns 时候
rather than 入时 across the word boundary.  Without one, at least two
characters, so a single-character correction never becomes a preference
for that syllable everywhere."
  (let ((n (length text)))
    (or (cl-loop for size from (max 2 (- end start)) to (min n nova-rime-max-word-syllables)
                 thereis (cdr (car (sort (cl-loop for s from (max 0 (- end size)) to (min start (- n size))
                                                  for entry = (nova-rime--dictionary-entry
                                                               (substring text s (+ s size))
                                                               (seq-subseq units s (+ s size)))
                                                  when entry collect (cons (nth 2 entry) (cons s (+ s size))))
                                         (lambda (a b) (> (car a) (car b)))))))
        (if (>= (- end start) 2) (cons start end)
          (let ((s (min start (- n 2)))) (cons s (+ s 2)))))))

(defun nova-rime--learn (text units default)
  "Forget readings TEXT contradicts under UNITS, then learn where it left DEFAULT."
  (when-let* ((db (nova-rime--learned-db)))
    (let ((n (length text)) (changed nil))
      (dotimes (s n)
        (cl-loop for e from (+ s 2) to (min n (+ s 16)) do
                 (let ((code (nova-rime--span-code units s e)) (span (substring text s e)))
                   (dolist (m (copy-sequence (gethash code nova-rime--memory)))
                     (unless (equal (car m) span)
                       (sqlite-execute db "DELETE FROM memory WHERE code = ? AND text = ?" (list code (car m)))
                       (setf (gethash code nova-rime--memory) (delete m (gethash code nova-rime--memory))
                             changed t))))))
      (when (and default (> n 1) (>= (length default) n) (not (equal (substring default 0 n) text)))
        (let ((i 0))
          (while (< i n)
            (if (eq (aref text i) (aref default i))
                (cl-incf i)
              (let ((j i))
                (while (and (< j n) (not (eq (aref text j) (aref default j))))
                  (cl-incf j))
                (pcase-let* ((`(,s . ,e) (nova-rime--learned-span text units i j))
                             (word (substring text s e)) (code (nova-rime--span-code units s e))
                             (aux (nova-rime--learned-aux word (seq-subseq units s e))))
                  (sqlite-execute db (concat "INSERT INTO memory VALUES (?, ?, ?, 1, ?) ON CONFLICT (code, text) "
                                             "DO UPDATE SET count = count + 1, updated = excluded.updated")
                                  (list code word aux (float-time)))
                  (unless (assoc word (gethash code nova-rime--memory))
                    (push (cons word aux) (gethash code nova-rime--memory)))
                  (setq changed t))
                (setq i j))))))
      (when changed (cl-incf nova-rime--memory-tick)))))

(defconst nova-rime--initials-expression
  (mapconcat (lambda (i) (format "substr(code,%d,1)" i)) '(1 3 5 7 9 11 13 15) "||")
  "SQL for a code's syllable initials, up to eight syllables.")

(defvar nova-rime--initials-index nil
  "Database file whose initials index `nova-rime--initials-index-p' checked:
\\(FILE . READY).")

(defun nova-rime--initials-index-p ()
  "Non-nil when the database has the index for abbreviated lookups.
Create it the first time, about a second and 30 MB on the full wanxiang
list; without it a pattern like \"w?x?h?\" scans every \"w\" code."
  (unless (equal (car nova-rime--initials-index) nova-rime-database-file)
    (setq nova-rime--initials-index
          (cons nova-rime-database-file
                (condition-case err
                    (progn
                      (sqlite-execute (nova-rime--database)
                                      (format "CREATE INDEX IF NOT EXISTS entry_initials ON entry(%s, weight DESC)"
                                              nova-rime--initials-expression))
                      t)
                  (error (message "nova-rime: no initials index, abbreviations are slow: %s" err)
                         nil)))))
  (cdr nova-rime--initials-index))

(defun nova-rime--pattern-initials (pattern)
  "Initials of PATTERN, whose syllables are two characters each."
  (cl-loop for i from 0 below (length pattern) by 2 concat (substring pattern i (1+ i))))

(defun nova-rime--pattern-entries (pattern)
  "Return dictionary entries whose code matches the GLOB PATTERN.
Each ``?'' in PATTERN is the final of a syllable typed as an initial."
  (when-let* ((db (nova-rime--database)))
    (if (nova-rime--initials-index-p)
        (sqlite-select db (format "SELECT word, aux, weight FROM entry INDEXED BY entry_initials WHERE %s = ? AND code GLOB ? ORDER BY weight DESC LIMIT ?"
                                  nova-rime--initials-expression)
                       (list (nova-rime--pattern-initials pattern) pattern nova-rime-database-limit))
      (sqlite-select db (concat "SELECT word, aux, weight FROM entry WHERE code GLOB ? "
                                "ORDER BY weight DESC LIMIT ?")
                     (list pattern nova-rime-database-limit)))))

(defun nova-rime--pattern-alive-p (pattern)
  "Non-nil when some dictionary code starts with PATTERN."
  (when-let* ((db (nova-rime--database)))
    (and (if (and (string-search "?" pattern) (nova-rime--initials-index-p))
             (let ((initials (nova-rime--pattern-initials pattern)))
               (sqlite-select db (format "SELECT 1 FROM entry INDEXED BY entry_initials WHERE %1$s >= ? AND %1$s < ? AND code GLOB ? LIMIT 1"
                                         nova-rime--initials-expression)
                              (list initials (concat initials "~") (concat pattern "*"))))
           (sqlite-select db "SELECT 1 FROM entry WHERE code GLOB ? LIMIT 1" (list (concat pattern "*"))))
         t)))

(defun nova-rime--entries-for (code)
  "Return the learned and dictionary word entries for CODE.
A CODE containing ``?'' comes from `nova-rime--span-code' over initials typed
alone, and matches every word whose syllables start with them."
  (and code (not (string-empty-p code))
       (if (string-search "?" code)
           (nova-rime--pattern-entries code)
         (append (nova-rime--memory-entries code) (nova-rime--database-entries code)))))

(defun nova-rime--matching-entries (entries units)
  "Return the entries of ENTRIES whose auxiliary codes match UNITS."
  (seq-filter (lambda (entry)
                (nova-rime--entry-match-p (nth 1 entry) units))
              entries))

(defun nova-rime--unit-words (unit)
  "Return the words for one UNIT that match its auxiliary code."
  (mapcar #'car (nova-rime--matching-entries (nova-rime--entries-for (car unit)) (list unit))))

(defun nova-rime--unit-pattern (unit)
  "UNIT's code for a dictionary lookup: an initial typed alone gets ``?''."
  (if (and (nova-rime--abbreviating-p) (= (length (car unit)) 1))
      (concat (car unit) "?")
    (car unit)))

(defun nova-rime--span-code (units start end)
  "Return the concatenated code of UNITS from START to END.
With `nova-rime-abbreviate', an initial typed alone becomes a GLOB pattern."
  (mapconcat #'nova-rime--unit-pattern (seq-subseq units start end) ""))

(defun nova-rime--abbreviating-p ()
  "Non-nil when `nova-rime-abbreviate' applies here; see its docstring."
  (and nova-rime-abbreviate (not (bound-and-true-p nova-rime-edit-mode))))

(defun nova-rime--unit-complete-p (unit)
  "Non-nil when UNIT can be decoded: a full syllable, or an initial
typed alone under `nova-rime-abbreviate'."
  (or (= (length (car unit)) 2)
      (and (nova-rime--abbreviating-p) (= (length (car unit)) 1))))

(defun nova-rime--best-paths (paths)
  "Deduplicate PATHS by surface, preserving dictionary evidence, then prune."
  (let ((seen (make-hash-table :test #'equal)) result)
    (dolist (path (cl-stable-sort paths #'> :key #'nova-rime-candidate-score))
      (let* ((text (nova-rime-candidate-text path)) (previous (gethash text seen)))
        (if previous
            (when (nova-rime-candidate-dictionary path)
              (setf (nova-rime-candidate-dictionary previous) t))
          (puthash text path seen)
          (push path result))))
    (seq-take (nreverse result) (max 1 nova-rime-search-width))))

(defun nova-rime--sentence-records (units &optional required-prefix)
  "Search UNITS, optionally requiring the output to start with REQUIRED-PREFIX.
Filter extensions before beam pruning, including constraints ending inside
a dictionary word.  Constrained searches never replace the typing cache.
Include the editable text before point in collocation scores and the cache
signature."
  (let* ((n (length units)) (context (nova-rime--context))
         (paths (make-vector (1+ n) nil))
         (signature (list context nova-rime-database-file nova-rime--db nova-rime-database-limit
                          nova-rime-word-penalty nova-rime-max-word-syllables nova-rime-search-width
                          nova-rime-grammar-module nova-rime-grammar-shared-dir nova-rime-grammar-language
                          nova-rime--grammar-ready nova-rime--grammar-failed nova-rime--pins nova-rime--typed-tones
                          nova-rime--memory-tick
                          (symbol-function 'nova-rime--entries-for)
                          (symbol-function 'nova-rime--grammar-score)))
         (cache (and (not required-prefix)
                     (equal signature (car nova-rime--search-cache)) nova-rime--search-cache))
         (old-units (nth 1 cache)) (old-paths (nth 2 cache))
         (common (cl-loop for old in old-units for new in units
                          while (equal old new) count t))
         ;; Only the last boundary uses octagram's rear score. When growing or
         ;; shrinking, rebuild both the old final boundary and the new one.
         (reuse (if (equal units old-units) n
                  (max 0 (min common (1- (length old-units)) (1- n))))))
    (aset paths 0 (list (nova-rime--make-candidate :text "" :score 0.0 :end 0)))
    (cl-loop for end from 1 to reuse do (aset paths end (aref old-paths end)))
    (cl-loop for end from (1+ reuse) to n do
             (let (extensions)
               (cl-loop for start from (max 0 (- end nova-rime-max-word-syllables))
                        to (1- end) do
                        (when (aref paths start)
                          (dolist (entry (nova-rime--matching-entries
                                          (nova-rime--entries-for
                                           (nova-rime--span-code units start end))
                                          (seq-subseq units start end)))
                            (dolist (path (aref paths start))
                              (let* ((word (car entry))
                                     (prefix (nova-rime-candidate-text path))
                                     (text (concat prefix word)))
                                (when (and (nova-rime--pins-allow-p word start)
                                           (nova-rime--tones-allow-p word start units)
                                           (or (not required-prefix)
                                               (string-prefix-p required-prefix text)
                                               (and (< end n)
                                                    (string-prefix-p text required-prefix))))
                                  (push (nova-rime--make-candidate
                                       :text text :end end
                                       :dictionary (= start 0)
                                       :score (+ (nova-rime-candidate-score path)
                                                 (log (max 1 (nth 2 entry)))
                                                 (- nova-rime-word-penalty)
                                                 (nova-rime--grammar-score (concat context prefix) word (= end n))))
                                      extensions)))))))
               (aset paths end (nova-rime--best-paths (nreverse extensions)))))
    (unless required-prefix (setq nova-rime--search-cache (list signature units paths)))
    ;; Full readings lead; shorter prefixes remain available after the beam.
    (let ((seen (make-hash-table :test #'equal)) result)
      (cl-loop for end from n downto 1 do
               (dolist (path (aref paths end))
                 (unless (gethash (nova-rime-candidate-text path) seen)
                   (puthash (nova-rime-candidate-text path) t seen)
                   (push path result))))
      (nreverse result))))

(defun nova-rime--prefix-entries (code)
  "Return completion entries whose code starts with CODE."
  (when-let* ((db (nova-rime--database)))
    (sqlite-select db
                   (concat "SELECT word, aux, weight FROM entry "
                           "WHERE code GLOB ? ORDER BY weight DESC LIMIT ?")
                   (list (concat code "*") (* 4 nova-rime-candidates-max)))))

(defun nova-rime--prefix-candidates (units)
  "Return completion candidates for an incomplete UNITS input.

A lone incomplete syllable can only complete to single characters; once
a complete syllable precedes it, longer words are allowed."
  (let ((entries (nova-rime--prefix-entries (nova-rime--full-code units))))
    (when (= (length units) 1)
      (setq entries (seq-filter (lambda (entry) (= (length (car entry)) 1))
                                entries)))
    (seq-uniq (mapcar #'car entries))))

(defun nova-rime--candidates (input)
  "Return candidates for the raw INPUT string.
Words the `tree' style froze join `nova-rime--pins' for the whole-input search."
  (let ((nova-rime--pins (nova-rime-tree--search-pins input)))
    (nova-rime--search-candidates input)))

(defun nova-rime--search-candidates (input)
  "Return candidates for the raw INPUT string.

Complete input, including a single syllable, uses contextual word scores.
An incomplete trailing syllable falls back to a prefix search, the way
Rime completes partial pinyin."
  (setq nova-rime--typed-tones (nova-rime--tones input))
  (let* ((units (nova-rime--parse input))
         (last (car (last units)))
         (complete (and units (cl-every #'nova-rime--unit-complete-p units)))
         (skip (run-hook-with-args-until-success 'nova-rime-lattice-inhibit-functions units)))
    (setq nova-rime--candidate-records
          (when (and units complete (not skip))
            ;; Preserve all display slots for single characters even with a
            ;; narrower sentence beam (e.g. width 1 or the default width 8).
            (let ((nova-rime-search-width (if (= (length units) 1)
                                        (max nova-rime-candidates-max nova-rime-search-width)
                                      nova-rime-search-width)))
              (nova-rime--sentence-records units))))
    (seq-take
     (cond
      ((or (null units) skip) nil)
      (nova-rime--candidate-records (mapcar #'nova-rime-candidate-text nova-rime--candidate-records))
      ((and last (not complete)) (nova-rime--prefix-candidates units))
      (t (seq-filter (lambda (word) (nova-rime--tones-allow-p word 0 units))
                     (nova-rime--unit-words (car units)))))
     nova-rime-candidates-max)))

(defun nova-rime--active-p ()
  "Return non-nil when a composition is in progress."
  (and nova-rime--input (not (string-empty-p nova-rime--input))))

(defun nova-rime--clear-overlay ()
  "Delete the preedit overlay, if any."
  (when (overlayp nova-rime--overlay)
    (delete-overlay nova-rime--overlay))
  (setq nova-rime--overlay nil))

(defun nova-rime--spans-p ()
  "Whether the current composition renders as one reading with word alternatives.
The `tree' style also covers a single syllable and an incomplete last one."
  (or (and (eq nova-rime-candidate-style 'tree) (not nova-rime--list-view)
           nova-rime--candidate-records
           (> (nova-rime-candidate-end (car nova-rime--candidate-records)) 1)
           (= (nova-rime-candidate-end (car nova-rime--candidate-records))
              (length (nova-rime--parse nova-rime--input))))
      (nova-rime-tree--tree-p)))

(defun nova-rime--format ()
  "Return the preedit string, including candidates when enabled."
  (setq nova-rime--span-choices nil)
  (concat
   (mapconcat (lambda (key)
                (if (nova-rime--tone-key-p key)
                    (propertize (string (aref nova-rime--tone-marks (- key ?1))) 'face 'nova-rime-aux-face)
                  (propertize (string key) 'face (if (nova-rime--aux-char-p key)
                                                     'nova-rime-aux-face
                                                   'nova-rime-preedit-face))))
              (or nova-rime--input "") "")
   (cond
    ((not (and nova-rime-show-candidates nova-rime--candidates)) nil)
    ((nova-rime--spans-p) (concat " " (car nova-rime--candidates)))
    (t (concat " " (nova-rime--format-list nova-rime--candidates nova-rime--selected))))))

(defun nova-rime--format-list (candidates selected)
  "CANDIDATES numbered from 1, the one at index SELECTED highlighted."
  (cl-loop for candidate in candidates for i from 0
           concat (propertize (format "%d.%s " (1+ i) candidate)
                              'face (if (= i selected) 'nova-rime-selected-face 'nova-rime-candidate-face))))

(defun nova-rime--display ()
  "Render the preedit and candidate list at point."
  (nova-rime--clear-overlay)
  (if (nova-rime--editing-p)
      (nova-rime-edit--render)
    (when (nova-rime--active-p)
      (let ((text (nova-rime--format)))
        (unless (string-empty-p text)
          (setq nova-rime--overlay (make-overlay (point) (point)))
          (overlay-put nova-rime--overlay 'after-string text))))))

(defun nova-rime--refresh ()
  "Recompute candidates from the raw input and refresh the display."
  (if (and (nova-rime--editing-p) (not (nova-rime-edit--decodable-p)))
      (nova-rime-edit--pending-refresh)
    (nova-rime--refresh-keys)))

(defun nova-rime--refresh-keys ()
  "Refresh a composition whose flattened input retains syllable boundaries."
  (let ((units (nova-rime--parse nova-rime--input)))
    ;; A pin survives only while the syllable it fixed keeps its keys.
    (setq nova-rime--pins (seq-filter (lambda (pin)
                                   (equal (car (nth (car pin) units)) (nth 2 pin)))
                                 nova-rime--pins)))
  (setq nova-rime--candidates (nova-rime--candidates nova-rime--input)
        nova-rime--selected 0)
  ;; A pinned refresh shows the user's own correction, not nova-rime's offer.
  (unless nova-rime--pins
    (setq nova-rime--default-reading (car nova-rime--candidates)))
  (nova-rime-active-mode (if (nova-rime--active-p) 1 -1))
  (nova-rime--display)
  (run-hooks 'nova-rime-candidates-updated-hook))

(defun nova-rime--reset ()
  "Discard the current composition and its display."
  (setq nova-rime--input nil
        nova-rime--candidate-records nil
        nova-rime--search-cache nil
        nova-rime--candidates nil
        nova-rime--span-choices nil
        nova-rime--pins nil
        nova-rime--typed-tones nil
        nova-rime--list-view nil
        nova-rime--default-reading nil
        nova-rime--selected 0)
  (nova-rime-active-mode -1)
  (nova-rime--clear-overlay)
  (run-hooks 'nova-rime-reset-hook))

(defvar-local nova-rime--committed-state nil
  "Confirmed input segment, endpoint, modification tick and dictionary.
The segment keeps actual keys and auxiliary input; ordinary history is text.")

(defun nova-rime--composition-state (&optional limit)
  "Return original history and any still-valid confirmed input segment.
History keeps at most LIMIT characters, as `nova-rime--context'."
  (or
   (pcase nova-rime--committed-state
     (`(,segment ,end ,tick ,database)
      (when (and (= end (point)) (= tick (buffer-chars-modified-tick))
                 (equal database nova-rime-database-file))
        (let* ((text (alist-get 'text segment))
               ;; Read past the confirmed segment so it cannot evict history.
               (visible (nova-rime--context (+ (or limit 64) (length text)))))
          (when (string-suffix-p text visible)
            `((context . ,(substring visible 0 (- (length visible) (length text))))
              (committed . ,segment)))))))
   `((context . ,(nova-rime--context limit)))))

(defun nova-rime--commit (text)
  "Commit TEXT, or the raw typed input when TEXT is nil."
  (let* ((fallback (or nova-rime--input ""))
         (units (nova-rime--parse fallback))
         (typed (seq-take units (length text)))
         (record (seq-find (lambda (c) (equal text (nova-rime-candidate-text c)))
                           nova-rime--candidate-records))
         (old (alist-get 'committed (nova-rime--composition-state)))
         (code (when (and record (string-match-p "\\`[一-鿿]+\\'" text)
                          (= (length text) (nova-rime-candidate-end record) (length typed))
                          (cl-every (lambda (u) (= (length (car u)) 2)) typed))
                 (nova-rime--full-code typed))))
    (when (and code (nova-rime--user-db))
      (sqlite-execute (nova-rime--user-db) "INSERT INTO commits VALUES (?, ?, ?, ?, ?, ?)"
                      (list (float-time) fallback code text (or nova-rime--default-reading "") (nova-rime--context)))
      (nova-rime--learn text typed nova-rime--default-reading))
    (nova-rime--reset)
    (insert (or text fallback))
    (setq nova-rime--committed-state
          (when code
            ;; Promote the complete old segment to history at this boundary,
            ;; rather than saving an arbitrary suffix of its keys.
            (when (> (+ (length (alist-get 'text old)) (length text)) 64)
              (setq old nil))
            (let ((aux (append (alist-get 'aux old)
                               (cl-loop for unit in typed for i from (length (alist-get 'text old))
                                        unless (string-empty-p (cdr unit))
                                        collect (vector i (downcase (cdr unit)))))))
              (list `((text . ,(concat (alist-get 'text old) text))
                      (input . ,(concat (alist-get 'input old) code))
                      (aux . ,(vconcat aux)))
                    (point) (buffer-chars-modified-tick) nova-rime-database-file))))))

(defun nova-rime--commit-selected ()
  "Commit the selected candidate once pending work has settled."
  (run-hooks 'nova-rime-settle-hook)
  (nova-rime--commit (nth nova-rime--selected nova-rime--candidates)))

(defun nova-rime--select-digit (key)
  "Apply the span alternative or commit the candidate numbered KEY.
In the span view only numbered alternatives respond; others do nothing."
  (run-hooks 'nova-rime-settle-hook)
  (let ((index (nova-rime--digit-index key)))
    (if (nova-rime--spans-p)
        (when-let* ((choice (nth index nova-rime--span-choices)))
          (apply #'nova-rime--choose-span choice))
      (when-let* ((text (nth index nova-rime--candidates)))
        (nova-rime--commit text)))))

(defun nova-rime--choose-span (start end text &optional split)
  "Fix TEXT at syllables START..END and decode the rest of the input again.
SPLIT is (START END WORD) when TEXT replaces one character of WORD; the
other characters of WORD stay as they are."
  (nova-rime-tree--choose-span start end split)
  (when (nova-rime--editing-p)
    (cl-incf nova-rime-edit--generation))
  (let ((units (nova-rime--parse nova-rime--input)))
    (setq nova-rime--pins
          (append (cl-loop for i from start below end
                           collect (list i (aref text (- i start)) (car (nth i units))))
                  (seq-remove (lambda (pin) (and (<= start (car pin)) (< (car pin) end)))
                              nova-rime--pins))))
  (nova-rime--refresh))

(defun nova-rime-toggle-candidate-list ()
  "Switch the current composition between span view and the sentence list."
  (interactive)
  (setq nova-rime--list-view (not nova-rime--list-view))
  (nova-rime--display))

(defun nova-rime--text-read-only-p ()
  "Return non-nil when inserting at point would fail."
  (and (or buffer-read-only (get-pos-property (point) 'read-only))
       (not (or inhibit-read-only
                (get-pos-property (point) 'inhibit-read-only)))))

(defun nova-rime--punctuation-for (key)
  "Return the full-width replacement for KEY, or nil."
  (unless nova-rime-ascii-punctuation
    (cdr (assq key nova-rime-punctuation))))

(defun nova-rime-toggle-punctuation ()
  "Toggle full-width punctuation."
  (interactive)
  (setq nova-rime-ascii-punctuation (not nova-rime-ascii-punctuation))
  (message "nova-rime punctuation: %s"
           (if nova-rime-ascii-punctuation "ASCII" "中文")))

(define-minor-mode nova-rime-english-mode
  "Insert keys literally while nova-rime stays the input method.
With editable preedit, text typed inside a composition stays part of it:
the Chinese on both sides converts as one sentence, and SPC after switching
back confirms everything."
  :lighter nil
  (force-mode-line-update))

(defun nova-rime-toggle-english (&optional arg)
  "Switch between Chinese and literal English without leaving composition.
Outside nova-rime, or with prefix ARG, run `toggle-input-method' instead."
  (interactive "P")
  (if (and (equal current-input-method "nova-rime") (not arg))
      (nova-rime-english-mode 'toggle)
    (toggle-input-method arg t)))

(defun nova-rime--english-input (key)
  "Insert KEY literally.
Inside an editable run KEY joins the run, which still converts as one
sentence.  The floating composition has no place for literal text, so it
commits its selected candidate first."
  (cond
   ((bound-and-true-p nova-rime-edit-mode)
    (unless (nova-rime-edit--type-literal key) (list key)))
   (t
    (when (nova-rime--active-p)
      (nova-rime--commit-selected))
    (list key))))

(defun nova-rime-input-method (key)
  "Process KEY for the wanxiang input method."
  (cond
   (nova-rime-raw-input (nova-rime-raw--input key))
   ((nova-rime--text-read-only-p) (list key))
   (nova-rime-english-mode (nova-rime--english-input key))
   ((bound-and-true-p nova-rime-edit-mode) (nova-rime-edit--input-method key))
   (t (nova-rime--legacy-input-method key))))

(defun nova-rime--legacy-input-method (key)
  "Process KEY with the floating composition interface."
  (cond
   ((or (nova-rime--code-char-p key (nova-rime--next-position))
        (and (nova-rime--aux-char-p key) (nova-rime--active-p)))
    (setq nova-rime--input (concat nova-rime--input (char-to-string key)))
    (nova-rime--refresh)
    nil)
   ((and (nova-rime--active-p) (nova-rime--digit-p key))
    (nova-rime--select-digit key)
    nil)
   ((and (nova-rime--active-p) (= key ?\s))
    (nova-rime--commit-selected)
    nil)
   ((nova-rime--punctuation-for key)
    (when (nova-rime--active-p)
      (nova-rime--commit-selected))
    (insert (nova-rime--punctuation-for key))
    nil)
   (t
    (when (nova-rime--active-p)
      (nova-rime--reset))
    (list key))))

(defun nova-rime-delete-backward ()
  "Delete the last typed key."
  (interactive)
  (if (nova-rime--editing-p)
      (nova-rime-edit-delete-backward)
  (when (and nova-rime--input (not (string-empty-p nova-rime--input)))
    (setq nova-rime--input (substring nova-rime--input 0 -1)))
  (if (nova-rime--active-p)
      (nova-rime--refresh)
    (nova-rime--reset))))

(defun nova-rime-commit-raw ()
  "Commit the typed input as literal text."
  (interactive)
  (nova-rime--commit nil))

(defun nova-rime-cancel ()
  "Cancel the current composition."
  (interactive)
  (nova-rime--reset))

(defun nova-rime--clear-before-unrelated-command ()
  "Drop the composition before a command that does not belong to nova-rime."
  (unless (or (bound-and-true-p nova-rime-edit-mode)
              (not (symbolp this-command))
              (string-prefix-p "nova-rime-" (symbol-name this-command))
              (string-match-p "self-insert" (symbol-name this-command)))
    (nova-rime--reset)))

(defvar nova-rime-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'nova-rime-delete-backward)
    (define-key map (kbd "<backspace>") #'nova-rime-delete-backward)
    (define-key map (kbd "RET") #'nova-rime-commit-raw)
    (define-key map (kbd "<return>") #'nova-rime-commit-raw)
    (define-key map (kbd "<escape>") #'nova-rime-cancel)
    (define-key map (kbd "TAB") #'nova-rime-toggle-candidate-list)
    (dotimes (i 5)
      (define-key map (kbd (format "C-%d" (1+ i))) #'nova-rime-tone))
    map)
  "Keymap installed while a nova-rime composition is active.")

(defvar-local nova-rime--active-emulation-map-alist nil
  "Editable composition bindings for this buffer, ahead of modal keymaps.")

(define-minor-mode nova-rime-active-mode
  "Minor mode installed while a nova-rime composition is active."
  :lighter nil
  :init-value nil
  :keymap nil
  (if nova-rime-active-mode
      (progn
        (if (bound-and-true-p nova-rime-edit-mode)
            (progn
              ;; Editable runs survive switching buffers.  A terminal-wide
              ;; override would keep intercepting keys in the destination.
              (setq nova-rime--active-emulation-map-alist
                    `((nova-rime-active-mode . ,nova-rime-edit--keymap)))
              (setq emulation-mode-map-alists
                    (cons 'nova-rime--active-emulation-map-alist
                          (remq 'nova-rime--active-emulation-map-alist
                                emulation-mode-map-alists))))
          (setq nova-rime--active-emulation-map-alist nil)
          (internal-push-keymap nova-rime-active-mode-map 'overriding-terminal-local-map))
        (add-hook 'pre-command-hook #'nova-rime--clear-before-unrelated-command nil t))
    (setq nova-rime--active-emulation-map-alist nil)
    (internal-pop-keymap nova-rime-active-mode-map 'overriding-terminal-local-map)
    (when (boundp 'nova-rime-edit--keymap)
      (internal-pop-keymap nova-rime-edit--keymap 'overriding-terminal-local-map))
    (remove-hook 'pre-command-hook #'nova-rime--clear-before-unrelated-command t)))

(define-minor-mode nova-rime-mode
  "Minor mode enabled while the nova-rime input method is active."
  :lighter nil
  :init-value nil
  :keymap nil)

(defun nova-rime-activate (_name)
  "Activate the nova-rime input method."
  ;; 树状候选和原始字母输入画树要用 nova-rime-edit 的排版函数。
  (when (or nova-rime-raw-input (eq nova-rime-candidate-style 'tree)) (require 'nova-rime-edit))
  (when (and nova-rime-editable-preedit (not nova-rime-raw-input)) (nova-rime-edit-mode 1))
  (add-hook 'post-command-hook #'nova-rime-tree--post-command nil t)
  (add-hook 'post-command-hook #'nova-rime-raw--post-command nil t)
  (add-hook 'nova-rime-reset-hook #'nova-rime-tree--reset nil t)
  (add-hook 'nova-rime-reset-hook #'nova-rime-raw--reset nil t)
  (setq-local input-method-function #'nova-rime-input-method)
  (setq-local deactivate-current-input-method-function #'nova-rime-deactivate)
  (nova-rime-english-mode -1)
  (nova-rime-mode 1))

(defun nova-rime-deactivate ()
  "Deactivate the nova-rime input method."
  (nova-rime--reset)
  (nova-rime-english-mode -1)
  (nova-rime-mode -1)
  (remove-hook 'post-command-hook #'nova-rime-tree--post-command t)
  (remove-hook 'post-command-hook #'nova-rime-raw--post-command t)
  (remove-hook 'nova-rime-reset-hook #'nova-rime-tree--reset t)
  (remove-hook 'nova-rime-reset-hook #'nova-rime-raw--reset t)
  (kill-local-variable 'input-method-function))

(defun nova-rime--minibuffer-inherit-english ()
  "Carry the opening buffer's English state into an inherited minibuffer.
`nova-rime-english-mode' is buffer-local, and activation in the fresh
minibuffer starts in Chinese.  Only entering the minibuffer inherits the
state: \\[nova-rime-toggle-english] pressed later inside it still starts
in Chinese."
  (when-let* (((equal current-input-method "nova-rime"))
              (window (minibuffer-selected-window))
              ((buffer-local-value 'nova-rime-english-mode (window-buffer window))))
    (nova-rime-english-mode 1)))

(add-hook 'minibuffer-setup-hook #'nova-rime--minibuffer-inherit-english)

(defun nova-rime-setup ()
  "Install nova-rime as the default input method."
  (setq default-input-method "nova-rime"))

(defun nova-rime-lighter ()
  "Return a mode-line indicator for nova-rime, or an empty string.

Bright while nova-rime handles input, dim while the buffer is read-only and
keys pass through."
  (if (and (equal current-input-method "nova-rime") (bound-and-true-p nova-rime-mode))
      (propertize (if nova-rime-english-mode nova-rime-english-title nova-rime-title)
                  'face (if (nova-rime--text-read-only-p)
                            'nova-rime-indicator-dim-face
                          'nova-rime-indicator-face)
                  'help-echo (if nova-rime-english-mode "nova-rime: 英文，C-\\ 切回中文"
                               "nova-rime: 中文双拼"))
    ""))

(register-input-method "nova-rime" "euc-cn" #'nova-rime-activate nova-rime-title
                       "Nova-rime double-pinyin input method.")

(autoload 'nova-rime-memory "nova-rime-memory" "Manage learned Nova-rime readings." t)
(autoload 'nova-rime-history "nova-rime-memory" "Browse Nova-rime commit history." t)
(autoload 'nova-rime-memory-at-point "nova-rime-memory" "Find learned readings near point." t)

(provide 'nova-rime)
;;; nova-rime.el ends here
