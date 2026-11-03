;;; nova-rime-edit.el --- Editable Chinese compositions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Buffer characters carry immutable reading records, so ordinary edits and
;; undo retain the readings of untouched characters.  Literal text separates
;; Chinese runs, except punctuation and `nova-rime-english-mode' text typed inside
;; a run: they become literal units of it, and the run converts as one
;; sentence around them.  Only the run at point participates in conversion;
;; switching input methods suspends conversion without throwing away those
;; records.

;;; Code:

(require 'nova-rime)
(require 'nova-rime-tree)

(declare-function evil-normal-state "evil-states" (&optional arg))

(defcustom nova-rime-edit-max-syllables 24
  "Maximum Chinese characters reconsidered together around point.
Literal text inside the run does not count."
  :type 'natnum :group 'nova-rime)

(defface nova-rime-edit-pending-face
  '((t (:inherit secondary-selection :underline t)))
  "Face for suspended Chinese text whose readings are still editable."
  :group 'nova-rime)

(defface nova-rime-edit-active-face
  '((t (:inherit highlight :underline t)))
  "Face for the Chinese text currently being converted."
  :group 'nova-rime)

(defface nova-rime-edit-aux-face
  '((t (:inherit nova-rime-aux-face :height 0.75 :weight bold)))
  "Face for auxiliary keys attached to their Chinese character."
  :group 'nova-rime)

(cl-defstruct (nova-rime-edit--unit (:constructor nova-rime-edit--make-unit))
  keys pin original (database nova-rime-database-file) literal)

(defvar-local nova-rime-edit--start nil)
(defvar-local nova-rime-edit--end nil)
(defvar-local nova-rime-edit--units nil)
(defvar-local nova-rime-edit--generation 0)
(defvar-local nova-rime-edit--decoration nil)
(defvar-local nova-rime-edit--pending-decorations nil)
(defvar-local nova-rime-edit--aux-decorations nil)
(defvar-local nova-rime-edit--correction-decorations nil)
(defvar-local nova-rime-edit--correction-layout nil)
(defvar-local nova-rime-edit--internal nil)
(defvar-local nova-rime-edit--undoing nil)
(defvar-local nova-rime-edit--paused-point nil)
(defvar nova-rime-edit-mode)

;; Plain insertions next to a Chinese character must not inherit its reading.
(add-to-list 'text-property-default-nonsticky '(nova-rime-edit-unit . t))

(defun nova-rime-edit--unit-at (position)
  "Return the reading at accessible POSITION, if any."
  (when (and (<= (point-min) position) (< position (point-max)))
    (let ((unit (get-text-property position 'nova-rime-edit-unit)))
      (and (nova-rime-edit--unit-p unit)
           (equal (nova-rime-edit--unit-database unit) nova-rime-database-file) unit))))

(defun nova-rime-edit--editable-unit-at (position)
  "The reading at accessible POSITION when the text there is editable and visible."
  (when-let* ((unit (nova-rime-edit--unit-at position)))
    (and (not (get-char-property position 'read-only)) (not (invisible-p position)) unit)))

(defun nova-rime-edit--unit-code (unit)
  "UNIT's double-pinyin code, without its auxiliary code and tone."
  (caar (nova-rime--parse (nova-rime-edit--unit-keys unit))))

(defun nova-rime-edit--pin-text (unit)
  "The character UNIT is pinned to, as a string, or nil."
  (when-let* ((pin (nova-rime-edit--unit-pin unit))) (string pin)))

(defun nova-rime-edit--active-p ()
  "Whether an editable Chinese run is currently selected."
  (and nova-rime-edit-mode (markerp nova-rime-edit--start)
       (eq (marker-buffer nova-rime-edit--start) (current-buffer))
       (eq (marker-buffer nova-rime-edit--end) (current-buffer))
       (cl-every (lambda (u) (equal (nova-rime-edit--unit-database u) nova-rime-database-file))
                 nova-rime-edit--units)))

(defun nova-rime-edit--writable-p ()
  "Whether the active run is accessible, visible and editable."
  (and (nova-rime-edit--active-p) (not buffer-read-only)
       (<= (point-min) nova-rime-edit--start nova-rime-edit--end (point-max))
       (cl-loop for p from (marker-position nova-rime-edit--start) below nova-rime-edit--end
                never (or (get-char-property p 'read-only) (invisible-p p)))))

(defun nova-rime-edit--clear-decoration ()
  (when (overlayp nova-rime-edit--decoration) (delete-overlay nova-rime-edit--decoration))
  (mapc #'delete-overlay nova-rime-edit--correction-decorations)
  (setq nova-rime-edit--decoration nil nova-rime-edit--correction-decorations nil
        nova-rime-edit--correction-layout nil nova-rime--span-choices nil))

(defun nova-rime-edit--refresh-aux-decorations ()
  "Show auxiliary keys and typed tones beside each reading-bearing character."
  (mapc #'delete-overlay nova-rime-edit--aux-decorations)
  (setq nova-rime-edit--aux-decorations nil)
  (when nova-rime-edit-mode
    (save-restriction
      (widen)
      (let ((position (point-min)))
        (while (< position (point-max))
          (let* ((unit (get-text-property position 'nova-rime-edit-unit))
                 (end (next-single-property-change position 'nova-rime-edit-unit nil (point-max)))
                 (keys (and (nova-rime-edit--unit-p unit) (not (nova-rime-edit--unit-literal unit))
                            (nova-rime-edit--unit-keys unit)))
                 (aux (and keys (concat (upcase (cdar (nova-rime--parse keys)))
                                        (mapconcat (lambda (tone) (string (aref nova-rime--tone-marks (1- (cdr tone)))))
                                                   (nova-rime--tones keys) "")))))
            (when (and aux (not (string-empty-p aux)))
              (cl-loop for p from position below end
                       do (let ((overlay (make-overlay p (1+ p))))
                            (overlay-put overlay 'priority 2)
                            (overlay-put overlay 'evaporate t)
                            (overlay-put overlay 'after-string
                                         (propertize aux 'face 'nova-rime-edit-aux-face
                                                     'display '(raise 0.3)))
                            (push overlay nova-rime-edit--aux-decorations))))
            (setq position end)))))))

(defun nova-rime-edit--refresh-pending-decorations ()
  "Mark every suspended reading span without changing buffer text.
Rebuild from reading properties so ordinary edits and undo remain the source
of truth.  Widen only for display; confirmation still respects narrowing."
  (nova-rime-edit--refresh-aux-decorations)
  (mapc #'delete-overlay nova-rime-edit--pending-decorations)
  (setq nova-rime-edit--pending-decorations nil)
  (when nova-rime-edit-mode
    (save-restriction
      (widen)
      (let* ((position (point-min))
             (active (and (nova-rime-edit--active-p) (< nova-rime-edit--start nova-rime-edit--end)))
             (active-start (and active (marker-position nova-rime-edit--start)))
             (active-end (and active (marker-position nova-rime-edit--end))))
        (while (< position (point-max))
          (cond
           ((and active-start (<= active-start position) (< position active-end))
            (setq position active-end))
           ((not (nova-rime-edit--unit-p (get-text-property position 'nova-rime-edit-unit)))
            (setq position (next-single-property-change position 'nova-rime-edit-unit nil (point-max))))
           (t
            (let ((start position))
              (while (and (< position (point-max))
                          (not (and active-start (= position active-start)))
                          (nova-rime-edit--unit-p
                           (get-text-property position 'nova-rime-edit-unit)))
                (setq position (next-single-property-change position 'nova-rime-edit-unit nil (point-max))))
              (let ((overlay (make-overlay start position nil nil nil)))
                (overlay-put overlay 'face 'nova-rime-edit-pending-face)
                (overlay-put overlay 'priority 1)
                (overlay-put overlay 'help-echo "暂存文字；移动光标继续修改，空格确认")
                (overlay-put overlay 'evaporate t)
                (push overlay nova-rime-edit--pending-decorations))))))))))

(defun nova-rime-edit--reset ()
  "Forget active conversion state, retaining buffer text and readings."
  (cl-incf nova-rime-edit--generation)
  (dolist (marker (list nova-rime-edit--start nova-rime-edit--end))
    (when (markerp marker) (set-marker marker nil)))
  (setq nova-rime-edit--start nil nova-rime-edit--end nil nova-rime-edit--units nil)
  (nova-rime-edit--clear-decoration)
  (nova-rime-edit--refresh-pending-decorations))

(defun nova-rime-edit-pause ()
  "Hide conversion choices, visibly marking the retained editable readings."
  (interactive)
  (nova-rime--reset)
  (setq nova-rime-edit--paused-point (point)))

(defun nova-rime-edit-escape ()
  "Suspend editable input and leave Evil insertion with the same key."
  (interactive)
  (nova-rime-edit-pause)
  (when (bound-and-true-p evil-local-mode)
    (evil-normal-state)))

(defun nova-rime-edit--bounds ()
  "Find the bounded Chinese run touching point, within the input area."
  (when-let* ((p (cond ((nova-rime-edit--editable-unit-at (point)) (point))
                       ((nova-rime-edit--editable-unit-at (1- (point))) (1- (point))))))
    (let ((start p) (end (1+ p)) (floor (nova-rime--input-start))
          (field (get-char-property p 'field))
          (limit (max 1 nova-rime-edit-max-syllables)))
      (while (and (> start floor) (nova-rime-edit--editable-unit-at (1- start))
                  (equal field (get-char-property (1- start) 'field)))
        (cl-decf start))
      (while (and (< end (point-max)) (nova-rime-edit--editable-unit-at end)
                  (equal field (get-char-property end 'field)))
        (cl-incf end))
      ;; A bounded window keeps idle decoding practical in long paragraphs.
      (let ((p (min end (point))) (n 0))
        (while (and (> p start)
                    (or (nova-rime-edit--unit-literal (nova-rime-edit--unit-at (1- p))) (< n limit)))
          (cl-decf p)
          (unless (nova-rime-edit--unit-literal (nova-rime-edit--unit-at p)) (cl-incf n)))
        (setq start p))
      (let ((p start) (n 0))
        (while (and (< p end)
                    (or (nova-rime-edit--unit-literal (nova-rime-edit--unit-at p)) (< n limit)))
          (unless (nova-rime-edit--unit-literal (nova-rime-edit--unit-at p)) (cl-incf n))
          (cl-incf p))
        (setq end p))
      ;; A run starts with a syllable; earlier literal text is context.
      (while (and (< start end) (nova-rime-edit--unit-literal (nova-rime-edit--unit-at start)))
        (cl-incf start))
      (when (and (< start end) (<= start (point)))
        (cons start end)))))

(defun nova-rime-edit--read-units (start end)
  (cl-loop for p from start below end collect (nova-rime-edit--unit-at p)))

(defun nova-rime-edit--syllables (units)
  "UNITS without literal text: what the engine converts."
  (seq-remove #'nova-rime-edit--unit-literal units))

(defun nova-rime-edit--literals ()
  "Literal text in the run as (SYLLABLES-BEFORE . TEXT) groups."
  (let ((n 0) groups)
    (dolist (unit nova-rime-edit--units (nreverse groups))
      (cond ((not (nova-rime-edit--unit-literal unit)) (cl-incf n))
            ((eql (caar groups) n)
             (setcdr (car groups) (concat (cdar groups) (nova-rime-edit--unit-keys unit))))
            (t (push (cons n (nova-rime-edit--unit-keys unit)) groups))))))

(defun nova-rime-edit--merge (text)
  "Run text: converted TEXT, one character per syllable, around the literals."
  (let ((i 0))
    (mapconcat (lambda (unit)
                 (if (nova-rime-edit--unit-literal unit) (nova-rime-edit--unit-keys unit)
                   (prog1 (substring text i (1+ i)) (cl-incf i))))
               nova-rime-edit--units "")))

(defun nova-rime-edit--offset (index)
  "Run offset of syllable INDEX, or the run's end past the last one."
  (or (nth index (cl-loop for unit in nova-rime-edit--units for offset from 0
                          unless (nova-rime-edit--unit-literal unit) collect offset))
      (length nova-rime-edit--units)))

(defun nova-rime-edit--load-input ()
  "Derive the existing engine's input and hard pins from reading records."
  (let ((syllables (nova-rime-edit--syllables nova-rime-edit--units)))
    (setq nova-rime--input (mapconcat #'nova-rime-edit--unit-keys syllables "")
          nova-rime--pins
          (cl-loop for unit in syllables for i from 0
                   when (nova-rime-edit--unit-pin unit)
                   collect (list i (nova-rime-edit--unit-pin unit)
                                 (nova-rime-edit--unit-code unit))))))

(defun nova-rime-edit--adopt (start end)
  "Select the reading-bearing buffer run START..END."
  (nova-rime--reset)
  (setq nova-rime-edit--start (copy-marker start)
        nova-rime-edit--end (copy-marker end t)
        nova-rime-edit--units (nova-rime-edit--read-units start end))
  (nova-rime-edit--load-input))

(defun nova-rime-edit--sync ()
  "Select the run at point, leaving literal text and suspended input alone."
  (when (and nova-rime-edit-mode nova-rime-mode (not nova-rime-edit--internal))
    (let ((bounds (nova-rime-edit--bounds)))
      (cond
       ((null bounds) (when nova-rime-edit--start (nova-rime--reset)))
       ((and (nova-rime-edit--active-p)
             (= (car bounds) nova-rime-edit--start) (= (cdr bounds) nova-rime-edit--end)) nil)
       (t
        (nova-rime-edit--adopt (car bounds) (cdr bounds))
        (if (nova-rime-edit--writable-p) (nova-rime--refresh) (nova-rime--reset)))))))

(defun nova-rime-edit--post-command ()
  (unless (memq this-command '(undo undo-only undo-redo))
    (setq nova-rime-edit--undoing nil))
  (unless (or nova-rime-edit--internal nova-rime-edit--undoing
              (eq (point) nova-rime-edit--paused-point)
              (memq this-command '(nova-rime-edit-pause nova-rime-edit-escape nova-rime-edit-finish
                                   nova-rime-edit-return toggle-input-method nova-rime-toggle-english
                                   undo undo-only undo-redo)))
    (nova-rime-edit--sync))
  (when (and (nova-rime-edit--active-p)
             (not (equal nova-rime-edit--correction-layout (nova-rime-edit--layout-signature))))
    (nova-rime-edit--render)))

(defun nova-rime-edit--before-change (_start _end)
  (unless nova-rime-edit--internal
    ;; All outstanding responses refer to the previous buffer contents.
    (when (nova-rime-edit--active-p) (nova-rime--reset))))

(defun nova-rime-edit--after-change (start end _old-length)
  (unless nova-rime-edit--internal
    (setq nova-rime-edit--undoing undo-in-progress)
    ;; Undo restores exact reading properties.  Other inserted/replaced text,
    ;; including yanked text, is literal and cannot acquire a stale reading.
    (unless undo-in-progress
      (let ((nova-rime-edit--internal t))
        (with-silent-modifications
          (remove-text-properties start end '(nova-rime-edit-unit nil)))))
    (nova-rime-edit--refresh-pending-decorations)))

(defun nova-rime-edit--candidate-text ()
  "Return a full reading, with a one-character fallback for incomplete keys.
With the `tree' style an incomplete trailing syllable keeps the shown text."
  (or (nova-rime-tree--pending-text)
      (let* ((syllables (nova-rime-edit--syllables nova-rime-edit--units))
             (n (length syllables))
             (candidate (seq-find
                         (lambda (c) (and (= (nova-rime-candidate-end c) n)
                                          (= (length (nova-rime-candidate-text c)) n)))
                         nova-rime--candidate-records))
             (guess (and candidate (nova-rime-candidate-text candidate)))
             (i -1))
        (cond
         ((not (nova-rime-edit--decodable-p))
          (mapconcat #'nova-rime-edit--unit-text syllables ""))
         (guess guess)
         (t
          (let (nova-rime--candidate-records nova-rime--search-cache nova-rime--pins)
            (mapconcat (lambda (unit)
                         (cl-incf i)
                         (or (nova-rime-edit--pin-text unit)
                             (car (nova-rime--candidates (nova-rime-edit--unit-keys unit)))
                             (substring (nova-rime-edit--unit-keys unit) 0 1)))
                       syllables "")))))))

(defun nova-rime-edit--decodable-p ()
  "Whether flattening this run preserves its syllable boundaries."
  (cl-every (lambda (u) (= (length (nova-rime-edit--unit-code u)) 2))
            (butlast (nova-rime-edit--syllables nova-rime-edit--units))))

(defun nova-rime-edit--pending-refresh ()
  "Display an incomplete interior syllable without changing its neighbors."
  (setq nova-rime--candidate-records nil nova-rime--candidates nil
        nova-rime--span-choices nil)
  (nova-rime-active-mode 1)
  (nova-rime--display)
  (run-hooks 'nova-rime-candidates-updated-hook))

(defun nova-rime-edit--shown (unit)
  "UNIT's character in the run, or nil when UNIT is not shown yet."
  (cl-loop for p from (marker-position nova-rime-edit--start) below nova-rime-edit--end
           when (eq (nova-rime-edit--unit-at p) unit) return (char-to-string (char-after p))))

(defun nova-rime-edit--unit-text (unit)
  "Keep an existing UNIT's displayed text while an interior key is pending."
  (or (nova-rime-edit--shown unit)
      (nova-rime-edit--pin-text unit)
      (substring (nova-rime-edit--unit-keys unit) 0 1)))

(defun nova-rime-edit--replace (text units &optional cursor)
  "Replace the selected run with TEXT and UNITS, preserving point and mark.
CURSOR, when provided, is the desired offset after a user input operation.
All real edits remain undoable; ordinary buffer modification hooks still run."
  (when (nova-rime-edit--writable-p)
    (let* ((start (marker-position nova-rime-edit--start))
           (end (marker-position nova-rime-edit--end))
           (old-length (- end start))
           (position (point)) (mark-position (mark t))
           (old (buffer-substring start end))
           (old-units (nova-rime-edit--read-units start end))
           (replacement (copy-sequence text))
           (nova-rime-edit--internal t))
      (cl-loop for unit in units for i from 0
               for source = (or (cl-position unit old-units :test #'eq)
                                (and (= old-length (length text)) i))
               do (when source
                    (set-text-properties i (1+ i) (text-properties-at source old) replacement))
               do (put-text-property i (1+ i) 'nova-rime-edit-unit unit replacement))
      (unless (equal-including-properties old replacement)
        (atomic-change-group
          (save-excursion
            (goto-char start)
            ;; Retain unrelated mode-owned properties on existing characters.
            (if (= old-length (length text))
                (cl-loop for i from 0 below old-length
                         for unit in units
                         do (unless (= (char-after (+ start i)) (aref text i))
                              (goto-char (+ start i))
                              (let ((props (text-properties-at (point))))
                                (delete-char 1)
                                (insert (apply #'propertize (substring text i (1+ i)) props))))
                         do (put-text-property (+ start i) (+ start i 1) 'nova-rime-edit-unit unit))
              (let ((prefix 0) (suffix 0) (new-length (length text)))
                (while (and (< prefix (min old-length new-length))
                            (equal-including-properties (substring old prefix (1+ prefix))
                                                        (substring replacement prefix (1+ prefix))))
                  (cl-incf prefix))
                (while (and (< suffix (- (min old-length new-length) prefix))
                            (equal-including-properties (substring old (- old-length suffix 1) (- old-length suffix))
                                                        (substring replacement (- new-length suffix 1) (- new-length suffix))))
                  (cl-incf suffix))
                (goto-char (+ start prefix))
                (delete-region (point) (- end suffix))
                (insert (substring replacement prefix (- new-length suffix))))))
          (set-marker nova-rime-edit--start start)
          (set-marker nova-rime-edit--end (+ start (length text)))))
      (setq nova-rime-edit--units units)
      (goto-char (cond (cursor (+ start cursor))
                       ((< position start) position)
                       ((<= position end) (+ start (min (- position start) (length text))))
                       (t (+ position (- (length text) old-length)))))
      (when (and (= old-length (length text)) mark-position (<= start mark-position end))
        (set-marker (mark-marker) (+ start (min (- mark-position start) (length text))))))))

(defun nova-rime-edit--layout-signature ()
  "Return the window geometry relevant to correction rows."
  (list (point-min) (point-max) (copy-tree face-remapping-alist)
        (mapcar (lambda (window)
                  (list window (window-body-width window) (window-hscroll window)
                        display-line-numbers
                        (with-selected-window window (line-number-display-width t))))
                (get-buffer-window-list (current-buffer) nil t))))

(defun nova-rime-edit--correction-groups (text)
  "Return (START END OPTIONS) groups, where OPTIONS contain (LABEL . TEXT).
Whole-sentence labels keep their original indices, including the reading
already shown in TEXT.  The tree view draws its own alternatives."
  (setq nova-rime--span-choices nil)
  (when (and nova-rime-show-candidates nova-rime--candidates)
    (unless (nova-rime--spans-p)
      (let ((label 0) options)
        (dolist (candidate nova-rime--candidates)
          (cl-incf label)
          (when (and (<= label 10) (= (length candidate) (length text)))
            (push (cons (% label 10) candidate) options)))
        (when options (list (list 0 (length text) (nreverse options))))))))

(defun nova-rime-edit--correction-label (option &optional window)
  "Return OPTION's number and dot, fitting one Han character in WINDOW."
  (let ((label (propertize (format "%d." (car option)) 'face 'nova-rime-label-face)))
    (when (and window (display-graphic-p (window-frame window))
               (> (length (cdr option)) 0)
               (> (char-width (aref (cdr option) 0)) 1))
      (let* ((width (nova-rime-edit--display-width label window))
             (limit (nova-rime-edit--display-width
                     (propertize (substring (cdr option) 0 1) 'face 'nova-rime-alternatives-face)
                     window)))
        ;; Rasterization can make two Latin glyphs a pixel wider than one Han
        ;; glyph even in the same monospaced font.  Shrink only that label,
        ;; remeasuring because font sizes are rounded to pixels too.
        (when (and (> limit 0) (> width limit))
          (cl-loop for scale = (/ (float limit) width) then (* scale 0.95)
                   repeat 20
                   do (put-text-property 0 (length label) 'face
                                         `(:inherit nova-rime-label-face :height ,scale) label)
                   until (<= (nova-rime-edit--display-width label window) limit)))))
    label))

(defun nova-rime-edit--correction-cell (option &optional window)
  (concat (nova-rime-edit--correction-label option window)
          (propertize (cdr option) 'face 'nova-rime-alternatives-face)))

(defun nova-rime-edit--correction-rows (columns &optional window)
  "Pack (COLUMN . OPTIONS) COLUMNS into the earliest free display rows.
Keep each group's options consecutive, measuring each cell in WINDOW so
fonts use the same units as the source-text alignment."
  (let (rows)
    (dolist (column columns)
      (let* ((left (car column))
             (cells (mapcar (lambda (option)
                              (let ((text (nova-rime-edit--correction-cell option window)))
                                (list left (nova-rime-edit--display-width text window) text)))
                            (cdr column)))
             (start 0))
        ;; A neighboring group can share these rows whenever its actual cells
        ;; fit.  Do not reserve a full-width band or an extra two-character gap.
        (while (cl-loop for cell in cells for row from start
                        thereis (cl-some
                                 (lambda (other)
                                   (and (< (car cell) (- (+ (car other) (cadr other)) 1e-6))
                                        (< (car other) (- (+ (car cell) (cadr cell)) 1e-6))))
                                 (nth row rows)))
          (cl-incf start))
        (setq rows (nconc rows (make-list (max 0 (- (+ start (length cells)) (length rows))) nil)))
        (cl-loop for cell in cells for row from start do (push cell (nth row rows)))))
    (mapconcat
     (lambda (row)
       (concat "\n"
               (mapconcat
                (lambda (cell)
                  (concat (propertize " " 'display `(space :align-to ,(car cell)))
                          (nth 2 cell)))
                (sort row (lambda (a b) (< (car a) (car b)))) "")))
     rows "")))

(defun nova-rime-edit--correction-location (position window)
  "Return (ANCHOR COLUMN SOFT-WRAP) for a correction at POSITION in WINDOW."
  (save-excursion
    (goto-char position)
    (let ((column (current-column)) (end (line-end-position)))
      (if (not window) (list end column nil)
        ;; Measure without our virtual candidate rows.  Normal text to the
        ;; right stays on its original screen line, above the candidates.
        (vertical-motion 0 window)
        (setq column
              (if (display-graphic-p (window-frame window))
                  ;; Pixel measurement includes the number gutter, while
                  ;; display-space alignment starts after it.
                  (/ (float (- (car (window-text-pixel-size window (point) position t))
                               (with-selected-window window (line-number-display-width t))))
                     (frame-char-width (window-frame window)))
                (- column (current-column))))
        (goto-char position)
        (vertical-motion 1 window)
        (list (min end (point)) column (< (point) end))))))

(defun nova-rime-edit--display-width (text window)
  "Measure TEXT in the units used by display-space alignment."
  (if (and window (display-graphic-p (window-frame window)))
      (with-selected-window window
        (/ (float
            (if (>= emacs-major-version 31)
                (string-pixel-width text (window-buffer window))
              ;; Emacs 29/30's string-pixel-width cannot inherit the source
              ;; buffer's text scaling.  Measure with those face remappings.
              (let ((remapping face-remapping-alist))
                (with-temp-buffer
                  (setq-local face-remapping-alist remapping
                              display-line-numbers nil line-prefix nil wrap-prefix nil)
                  (insert text)
                  (car (buffer-text-pixel-size nil nil t))))))
           (frame-char-width (window-frame window))))
    (string-width text)))

(defun nova-rime-edit--show-corrections (text)
  "Display correction columns underneath their source text."
  (let ((groups (nova-rime-edit--correction-groups text))
        (windows (or (get-buffer-window-list (current-buffer) nil t) '(nil))))
    (dolist (group groups)
      (let ((overlay (make-overlay (+ nova-rime-edit--start (nova-rime-edit--offset (car group)))
                                   (+ nova-rime-edit--start 1 (nova-rime-edit--offset (1- (nth 1 group)))))))
        (overlay-put overlay 'face 'nova-rime-uncertain-face)
        (overlay-put overlay 'priority 1)
        (push overlay nova-rime-edit--correction-decorations)))
    (dolist (window windows)
      (let (lines)
        ;; Measure every column before inserting any virtual lines.
        (dolist (group groups)
          (pcase-let* ((`(,anchor ,column ,soft)
                        (nova-rime-edit--correction-location
                         (+ nova-rime-edit--start (nova-rime-edit--offset (car group))) window))
                       (options (nth 2 group))
                       (width (apply #'max (mapcar (lambda (option)
                                                    (nova-rime-edit--display-width
                                                     (nova-rime-edit--correction-cell option window) window))
                                                  options)))
                       ;; Put the alternative's characters under the source;
                       ;; its number occupies the small gutter to the left.
                       (column (max 0 (- column (nova-rime-edit--display-width
                                                 (nova-rime-edit--correction-label (car options) window)
                                                 window))))
                       (column (if window
                                   (max 0 (min column (- (window-body-width window)
                                                        (with-selected-window window
                                                          (line-number-display-width 'columns))
                                                        width 1)))
                                 column))
                       (line (assq anchor lines)))
            (unless line
              (setq line (list anchor soft nil))
              (push line lines))
            (setf (nth 2 line) (append (nth 2 line) (list (cons column options))))))
        (dolist (line lines)
          (let ((overlay (make-overlay (car line) (car line))))
            (when window (overlay-put overlay 'window window))
            (overlay-put overlay 'after-string
                         (concat (nova-rime-edit--correction-rows (nth 2 line) window)
                                 (when (nth 1 line) "\n")))
            (push overlay nova-rime-edit--correction-decorations))))))
  (setq nova-rime-edit--correction-layout (nova-rime-edit--layout-signature)))

(defun nova-rime-edit--render ()
  "Publish a reading in the buffer and show non-textual conversion details."
  (when (nova-rime-edit--writable-p)
    (let* ((text (nova-rime-edit--candidate-text))
           (i -1)
           (units (mapcar
                   (lambda (unit)
                     (if (nova-rime-edit--unit-literal unit) unit
                       (let ((pin (nth 1 (assq (cl-incf i) nova-rime--pins))))
                         (if (and (nova-rime-edit--unit-original unit)
                                  (equal pin (nova-rime-edit--unit-pin unit))) unit
                           (nova-rime-edit--make-unit
                            :keys (nova-rime-edit--unit-keys unit) :pin pin
                            :original (or (nova-rime-edit--unit-original unit) (aref text i)))))))
                   nova-rime-edit--units)))
      (nova-rime-edit--replace (nova-rime-edit--merge text) units)
      (nova-rime-edit--clear-decoration)
      (setq nova-rime-edit--decoration (make-overlay nova-rime-edit--start nova-rime-edit--end))
      (overlay-put nova-rime-edit--decoration 'face 'nova-rime-edit-active-face)
      (overlay-put nova-rime-edit--decoration 'help-echo
                   "空格确认，ESC 暂存；TAB 切换整句候选")
      (nova-rime-edit--refresh-pending-decorations)
      (if (eq nova-rime-candidate-style 'tree)
          (nova-rime-tree--show-corrections text)
        (nova-rime-edit--show-corrections text)))))

(defun nova-rime-edit--refresh (units cursor)
  "Update reading UNITS and place point at the syllable boundary CURSOR."
  (cl-incf nova-rime-edit--generation)
  (setq nova-rime-edit--units units nova-rime--search-cache nil)
  (nova-rime-edit--load-input)
  (cond
   ((nova-rime-edit--syllables units)
    (nova-rime--refresh)
    (goto-char (+ nova-rime-edit--start cursor)))
   (t
    ;; Without syllables, remaining literal units are ordinary text.
    (nova-rime-edit--replace (mapconcat #'nova-rime-edit--unit-keys units "") units cursor)
    (let ((nova-rime-edit--internal t))
      (with-silent-modifications
        (remove-text-properties nova-rime-edit--start nova-rime-edit--end '(nova-rime-edit-unit nil))))
    (nova-rime--reset))))

(defun nova-rime-edit--type-code (key)
  (nova-rime-edit--sync)
  (unless (nova-rime-edit--active-p) (nova-rime-edit--adopt (point) (point)))
  (let* ((offset (- (point) nova-rime-edit--start))
         (previous (and (> offset 0) (nth (1- offset) nova-rime-edit--units)))
         (partial (and previous (not (nova-rime-edit--unit-literal previous))
                       (= (length (nova-rime-edit--unit-code previous)) 1)))
         (index (if partial (1- offset) offset))
         (unit (nova-rime-edit--make-unit
                :keys (concat (and partial (nova-rime-edit--unit-keys previous)) (char-to-string key))))
         (units (append (seq-take nova-rime-edit--units index) (list unit)
                        (nthcdr (if partial offset index) nova-rime-edit--units))))
    (nova-rime-edit--refresh units (1+ index))))

(defun nova-rime-edit--type-aux (key)
  "Attach KEY to the character immediately before point."
  (nova-rime-edit--sync)
  (when (and (nova-rime-edit--active-p) (> (point) nova-rime-edit--start))
    (let* ((index (1- (- (point) nova-rime-edit--start)))
           (unit (nth index nova-rime-edit--units)))
      (when (and (not (nova-rime-edit--unit-literal unit))
                 (= (length (nova-rime-edit--unit-code unit)) 2))
        (nova-rime-edit--refresh
         (append (seq-take nova-rime-edit--units index)
                 (list (nova-rime-edit--make-unit
                        :keys (concat (nova-rime-edit--unit-keys unit) (char-to-string key))
                        :original (nova-rime-edit--unit-original unit)))
                 (nthcdr (1+ index) nova-rime-edit--units)) (1+ index))
        t))))

(defun nova-rime-edit--type-tone (tone)
  "Type TONE for the syllable immediately before point."
  (nova-rime-edit--sync)
  (when (and (nova-rime-edit--active-p) (> (point) nova-rime-edit--start))
    (let* ((index (1- (- (point) nova-rime-edit--start)))
           (unit (nth index nova-rime-edit--units)))
      (unless (nova-rime-edit--unit-literal unit)
        (nova-rime-edit--refresh
         (append (seq-take nova-rime-edit--units index)
                 (list (nova-rime-edit--make-unit :keys (nova-rime--retone (nova-rime-edit--unit-keys unit) tone)
                                             :original (nova-rime-edit--unit-original unit)))
                 (nthcdr (1+ index) nova-rime-edit--units))
         (1+ index))))))

(defun nova-rime-edit--type-literal (text)
  "Insert TEXT, a string or one key, as literal units of the run at point.
Return nil without a run; text before its first syllable is context."
  (setq nova-rime-edit--undoing nil nova-rime-edit--paused-point nil)
  (nova-rime-edit--sync)
  (when (and (nova-rime-edit--active-p) (> (point) nova-rime-edit--start))
    (let ((offset (- (point) nova-rime-edit--start)))
      (nova-rime-edit--refresh
       (append (seq-take nova-rime-edit--units offset)
               (mapcar (lambda (char) (nova-rime-edit--make-unit :keys (string char) :literal t))
                       (if (stringp text) text (string text)))
               (nthcdr offset nova-rime-edit--units))
       (+ offset (if (stringp text) (length text) 1)))
      t)))

(defun nova-rime-edit--choose (text)
  "Fix the explicitly chosen full TEXT without ending its editable run."
  (when (= (length text) (length (nova-rime-edit--syllables nova-rime-edit--units)))
    (cl-incf nova-rime-edit--generation)
    (setq nova-rime--pins
          (cl-loop for unit in (nova-rime-edit--syllables nova-rime-edit--units) for i from 0
                   collect (list i (aref text i) (nova-rime-edit--unit-code unit))))
    (nova-rime--refresh)))

(defun nova-rime-edit--input-method (key)
  (setq nova-rime-edit--undoing nil nova-rime-edit--paused-point nil)
  (cond
   ((nova-rime--code-char-p
     key (if (let ((unit (nova-rime-edit--unit-at (1- (point)))))
               (and unit (not (nova-rime-edit--unit-literal unit))
                    (= (length (nova-rime-edit--unit-code unit)) 1)))
             'final 'initial))
    (nova-rime-edit--type-code key) nil)
   ((and (nova-rime--aux-char-p key) (nova-rime-edit--type-aux key)) nil)
   ((and (nova-rime-edit--active-p) (nova-rime--digit-p key))
    (when nova-rime-show-candidates
      (if (nova-rime--spans-p) (nova-rime--select-digit key)
        (run-hooks 'nova-rime-settle-hook)
        (when-let* ((text (nth (nova-rime--digit-index key) nova-rime--candidates)))
          (nova-rime-edit--choose text))))
    nil)
   ((and (= key ?\s)
         (text-property-not-all (point-min) (point-max) 'nova-rime-edit-unit nil))
    (nova-rime-edit-finish) nil)
   ((nova-rime--punctuation-for key)
    ;; Punctuation continues the sentence; only SPC or ESC ends conversion.
    (unless (nova-rime-edit--type-literal (nova-rime--punctuation-for key))
      (insert (nova-rime--punctuation-for key)))
    nil)
   (t (nova-rime-edit-pause) (list key))))

(defun nova-rime-edit-delete-backward ()
  "Remove one auxiliary/key input at the character before point."
  (interactive)
  (nova-rime-edit--sync)
  (if (and (nova-rime-edit--active-p) (> (point) nova-rime-edit--start))
      (let* ((index (1- (- (point) nova-rime-edit--start)))
             (unit (nth index nova-rime-edit--units))
             (keys (substring (nova-rime-edit--unit-keys unit) 0 -1)))
        (nova-rime-edit--refresh
         (append (seq-take nova-rime-edit--units index)
                 (unless (string-empty-p keys) (list (nova-rime-edit--make-unit :keys keys)))
                 (nthcdr (1+ index) nova-rime-edit--units))
         (+ index (if (string-empty-p keys) 0 1))))
    (backward-delete-char-untabify 1)))

(defun nova-rime-edit-finish ()
  "Confirm pending readings in the accessible buffer, retaining its text."
  (interactive)
  (run-hooks 'nova-rime-settle-hook)
  (nova-rime-edit-pause)
  (let ((nova-rime-edit--internal t) (inhibit-read-only t) (position (point-min)))
    ;; Literal units become ordinary text; the Chinese around them is
    ;; recorded and learned as separate runs.
    (while (< position (point-max))
      (let ((end (next-single-property-change position 'nova-rime-edit-unit nil (point-max)))
            (unit (get-text-property position 'nova-rime-edit-unit)))
        (when (and (nova-rime-edit--unit-p unit) (nova-rime-edit--unit-literal unit))
          (remove-text-properties position end '(nova-rime-edit-unit nil)))
        (setq position end))))
  (let ((nova-rime-edit--internal t))
    ;; An unmatched or unfinished syllable is still input.  Do not discard
    ;; its remaining keys when removing the per-character reading record.
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((unit (nova-rime-edit--unit-at (point))))
          (if (and unit (not (<= #x3400 (char-after) #x9fff))
                   (not (get-char-property (point) 'read-only))
                   (not buffer-read-only))
              (let ((keys (nova-rime-edit--unit-keys unit)))
                (atomic-change-group (delete-char 1) (insert keys)))
            (forward-char)))))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (if (not (nova-rime-edit--unit-at (point))) (forward-char)
          (let ((start (point)))
            (while (and (< (point) (point-max)) (nova-rime-edit--unit-at (point)))
              (forward-char))
            (let* ((end (point))
                   (units (nova-rime-edit--read-units start end))
                   (raw (mapconcat #'nova-rime-edit--unit-keys units ""))
                   (parsed (nova-rime--parse raw))
                   (text (buffer-substring-no-properties start end))
                   (original (concat (cl-loop for u in units for i from 0
                                              collect (or (nova-rime-edit--unit-original u) (aref text i))))))
              (when (and (= (length parsed) (length text))
                         (cl-every (lambda (u) (= (length (car u)) 2)) parsed)
                         (string-match-p "\\`[一-鿿]+\\'" text)
                         (nova-rime--user-db))
                (sqlite-execute (nova-rime--user-db) "INSERT INTO commits VALUES (?, ?, ?, ?, ?, ?)"
                                (list (float-time) raw (nova-rime--full-code parsed) text original
                                      (save-excursion (goto-char start) (nova-rime--context-at-point))))
                ;; Model revisions are not user corrections.  Only explicitly
                ;; pinned/aux-filtered choices teach the reading preference.
                (let ((baseline
                       (concat
                        (cl-loop for u in units for (_code . aux) in parsed for i from 0
                                 collect (if (or (nova-rime-edit--unit-pin u) (not (string-empty-p aux)))
                                             (aref original i) (aref text i))))))
                  (unless (equal text baseline)
                    (nova-rime--learn text parsed baseline)))))))))
    (atomic-change-group
      (let ((position (point-min)))
        (while (< position (point-max))
          (let ((end (next-single-property-change position 'nova-rime-edit-unit nil (point-max))))
            (when (get-text-property position 'nova-rime-edit-unit)
              ;; Only discard our annotation, including on text a mode has
              ;; since protected.  The text and its protection stay unchanged.
              (let ((inhibit-read-only t))
                (remove-text-properties position end '(nova-rime-edit-unit nil))))
            (setq position end))))))
  (nova-rime-edit--refresh-pending-decorations))

(defun nova-rime-edit-return ()
  "Commit the active run as raw keys, or confirm and run normal Return."
  (interactive)
  (if (nova-rime-edit--active-p)
      (when (nova-rime-edit--writable-p)
        (let ((start (marker-position nova-rime-edit--start))
              (end (marker-position nova-rime-edit--end))
              (raw (mapconcat #'nova-rime-edit--unit-keys nova-rime-edit--units "")))
          (nova-rime-edit-pause)
          (atomic-change-group
            (delete-region start end)
            (goto-char start)
            (insert raw))))
    (nova-rime-edit-finish)
    (let* ((nova-rime-edit-mode nil) (command (key-binding (kbd "RET"))))
      (when (and (commandp command) (not (eq command #'nova-rime-edit-return)))
        (call-interactively command)))))

(defvar nova-rime-edit--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'nova-rime-edit-delete-backward)
    (define-key map (kbd "<backspace>") #'nova-rime-edit-delete-backward)
    (define-key map (kbd "RET") #'nova-rime-edit-return)
    (define-key map (kbd "<return>") #'nova-rime-edit-return)
    (define-key map (kbd "TAB") #'nova-rime-toggle-candidate-list)
    (define-key map (kbd "<escape>") #'nova-rime-edit-escape)
    (dotimes (i 5)
      (define-key map (kbd (format "C-%d" (1+ i))) #'nova-rime-tone))
    map))

(defvar nova-rime-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nova-rime-edit-return)
    (define-key map (kbd "<return>") #'nova-rime-edit-return)
    map))

;;;###autoload
(define-minor-mode nova-rime-edit-mode
  "Keep Chinese readings attached to editable buffer characters.
SPC confirms pending text; ESC suspends it with a visible marker.
Moving back to a character resumes conversion; uppercase auxiliary codes
apply to the character before point.  Switching input methods preserves
readings for later edits."
  :lighter " 半"
  (if nova-rime-edit-mode
      (progn
        (add-hook 'nova-rime-reset-hook #'nova-rime-edit--reset nil t)
        (add-hook 'post-command-hook #'nova-rime-edit--post-command nil t)
        (add-hook 'before-change-functions #'nova-rime-edit--before-change nil t)
        (add-hook 'after-change-functions #'nova-rime-edit--after-change nil t)
        (nova-rime-edit--refresh-pending-decorations))
    (nova-rime-edit-finish)
    (remove-hook 'nova-rime-reset-hook #'nova-rime-edit--reset t)
    (remove-hook 'post-command-hook #'nova-rime-edit--post-command t)
    (remove-hook 'before-change-functions #'nova-rime-edit--before-change t)
    (remove-hook 'after-change-functions #'nova-rime-edit--after-change t)))

(provide 'nova-rime-edit)
;;; nova-rime-edit.el ends here
