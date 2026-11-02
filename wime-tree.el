;;; wime-tree.el --- Tree view of the current reading -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of nova-rime, a fork of wime.  Splits the best reading into word
;; chips joined by a line, lists alternatives under each chip, and lets a
;; digit fix one chip while the rest is decoded again.  Needs no neural model:
;; chips come from dictionary word boundaries and differences among candidates.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'wime)

(defvar wime-edit--start)
(defvar wime-edit--correction-decorations)
(defvar wime-edit--correction-layout)
(declare-function wime-edit--offset "wime-edit")
(declare-function wime-edit--display-width "wime-edit")
(declare-function wime-edit--correction-location "wime-edit")
(declare-function wime-edit--layout-signature "wime-edit")

(defface wime-tree-chip-face
  '((t :background "#2f6fde" :foreground "white" :weight bold))
  "A word chip of the current reading."
  :group 'wime)

(defface wime-tree-line-face
  '((t :strike-through "#8a8a8a"))
  "The line joining word chips."
  :group 'wime)

(defface wime-tree-branch-face
  '((t :foreground "#8a8a8a"))
  "The root and the branch before an alternative."
  :group 'wime)

(defun wime-tree--crosses-p (top other position)
  "Whether a difference between TOP and OTHER crosses POSITION."
  (and (< 0 position (length top))
       (not (eq (aref top (1- position)) (aref other (1- position))))
       (not (eq (aref top position) (aref other position)))))

(defun wime-tree--segments (top others word-ends)
  "Split TOP at WORD-ENDS into a list of (START END ALTERNATIVES ...).
OTHERS are readings of the same length, best first.  A reading whose difference
crosses a chip boundary is not an alternative of that chip: merging chips for it
would collapse the sentence into one chip.  TAB still reaches it in the list."
  (let ((start 0) segments)
    (dolist (end word-ends)
      (let* ((word (substring top start end))
             (parts (cl-loop for other in others
                             unless (or (wime-tree--crosses-p top other start)
                                        (wime-tree--crosses-p top other end))
                             collect (substring other start end)))
             (alternatives (seq-uniq (remove word parts))))
        (push (cons start
                    (cons end (seq-take alternatives (max 1 (1- wime-candidates-max)))))
              segments))
      (setq start end))
    (nreverse segments)))

(defun wime-tree--word-ends (top units)
  "Return word ends in TOP, taking the longest dictionary word from the left."
  (let ((n (length top)) (start 0) ends)
    (while (< start n)
      (let ((end (or (cl-loop for end from (min n (+ start wime-max-word-syllables))
                              above (1+ start)
                              when (assoc (substring top start end)
                                          (wime--entries-for (wime--span-code units start end)))
                              return end)
                     (1+ start))))
        (push end ends)
        (setq start end)))
    (nreverse ends)))

(defun wime-tree--label (segments)
  "Number the alternatives of SEGMENTS, returning ((START END (LABEL . TEXT) ...) ...).
Also set `wime--span-choices', which digits select from in this order."
  (let ((label 0) choices)
    (prog1
        (mapcar (lambda (segment)
                  (pcase-let ((`(,start ,end . ,alternatives) segment))
                    (cons start
                          (cons end
                                (cl-loop for alternative in alternatives
                                         while (< label 10)
                                         do (cl-incf label)
                                         (push (list start end alternative) choices)
                                         collect (cons (% label 10) alternative))))))
                segments)
      (setq wime--span-choices (nreverse choices)))))

(defun wime-tree--align (column &optional face)
  (propertize " " 'display `(space :align-to ,column) 'face face))

(defun wime-tree--chunk (columns)
  "Render COLUMNS, each (LEFT CHIP CELLS), as chips with alternatives below."
  (let* ((root (propertize "●" 'face 'wime-tree-branch-face))
         (chips (cl-loop for (left chip) in columns
                         for first = t then nil
                         concat (concat (if first
                                            (concat (wime-tree--align (- left 2)) root)
                                          "")
                                        (wime-tree--align left 'wime-tree-line-face)
                                        chip)))
         (height (apply #'max (mapcar (lambda (column) (length (nth 2 column))) columns))))
    (concat
     "\n" chips
     (mapconcat
      (lambda (row)
        (concat "\n"
                (cl-loop for (left _chip cells) in columns
                         for cell = (nth row cells)
                         when cell
                         concat (concat (wime-tree--align left) cell))))
      (number-sequence 0 (1- height)) ""))))

(defun wime-tree--rows (text groups left window)
  "Lay GROUPS out from column LEFT, wrapping to a new row when needed."
  (let* ((limit (if window
                    (- (window-body-width window)
                       (with-selected-window window (line-number-display-width 'columns))
                       1)
                  most-positive-fixnum))
         (first-left (+ (max left 0) 2))
         (gap (wime-edit--display-width "──" window))
         (branch (propertize "╰" 'face 'wime-tree-branch-face))
         (x first-left) chunks chunk)
    (pcase-dolist (`(,start ,end . ,options) groups)
      (let* ((chip (propertize (concat " " (substring text start end) " ")
                               'face 'wime-tree-chip-face))
             (cells (mapcar (lambda (option)
                              (concat branch
                                      (propertize (number-to-string (car option))
                                                  'face 'wime-label-face)
                                      (propertize (cdr option)
                                                  'face 'wime-alternatives-face)))
                            options))
             (width (apply #'max (wime-edit--display-width chip window)
                           (mapcar (lambda (cell) (wime-edit--display-width cell window))
                                   cells))))
        (when (and chunk (> (+ x width) limit))
          (push (nreverse chunk) chunks)
          (setq chunk nil x 2))
        (push (list x chip cells) chunk)
        (setq x (+ x width gap))))
    (when chunk (push (nreverse chunk) chunks))
    (mapconcat #'wime-tree--chunk (nreverse chunks) "")))

(defun wime-tree--groups (text)
  "Compute the tree of TEXT and set what each digit selects."
  (let* ((n (length text))
         (units (wime--parse wime--input))
         (others (cl-loop for record in wime--candidate-records
                          for reading = (wime-candidate-text record)
                          when (and (= (length reading) n) (not (equal reading text)))
                          collect reading)))
    (wime-tree--label
     (wime-tree--segments text others (wime-tree--word-ends text units)))))

(defun wime-tree--show (text)
  "Draw the tree under TEXT and mark chips that have alternatives."
  (let ((groups (when wime-show-candidates (wime-tree--groups text)))
        (windows (or (get-buffer-window-list (current-buffer) nil t) '(nil))))
    (when groups
      (pcase-dolist (`(,start ,end . ,options) groups)
        (when options
          (let ((overlay (make-overlay (+ wime-edit--start (wime-edit--offset start))
                                       (+ wime-edit--start 1 (wime-edit--offset (1- end))))))
            (overlay-put overlay 'face 'wime-uncertain-face)
            (overlay-put overlay 'priority 1)
            (push overlay wime-edit--correction-decorations))))
      (dolist (window windows)
        (pcase-let* ((`(,start-anchor ,start-column ,_)
                      (wime-edit--correction-location wime-edit--start window))
                     (`(,anchor ,_ ,soft)
                      (wime-edit--correction-location
                       (+ wime-edit--start (wime-edit--offset (1- (length text)))) window))
                     (left (if (eq start-anchor anchor) start-column 0))
                     (overlay (make-overlay anchor anchor)))
          (when window (overlay-put overlay 'window window))
          (overlay-put overlay 'after-string
                       (concat (wime-tree--rows text groups left window)
                               (when soft "\n")))
          (push overlay wime-edit--correction-decorations)))))
  (setq wime-edit--correction-layout (wime-edit--layout-signature)))

(provide 'wime-tree)
;;; wime-tree.el ends here
