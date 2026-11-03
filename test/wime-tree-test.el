;;; wime-tree-test.el --- Tree candidates and raw-letter input -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'wime-edit)
(require 'wime-tree)
(require 'wime-raw)

;; `wime-edit-test--buffer' and its dictionary come from wime-edit-test.el,
;; loaded first by run.el.  甲 and 乙 share "aa" at 100 and 90, so 甲 is an
;; uncertain word and lists 乙 with a sentence-wide label.

(defun wime-tree-test--tree ()
  (cl-some (lambda (overlay) (overlay-get overlay 'after-string))
           wime-edit--correction-decorations))

(ert-deftest wime-tree-shows-chips-and-digit-fixes-one ()
  "The tree needs no reranker; a digit fixes one chip and keeps composing."
  (wime-edit-test--buffer
    (let ((wime-candidate-style 'tree) (wime-tree-show-keys nil))
      (wime-edit-test--type "aabb")
      (should (wime--spans-p))
      (let ((tree (wime-tree-test--tree)))
        (should (string-search " 甲 " tree))
        (should (string-search " 丙 " tree))
        (should (string-search "╰1" tree)))
      (let ((choice (car wime--span-choices)))
        (should (equal (nth 2 choice) "乙"))
        (wime-edit-test--type "1")
        (should (string-search "乙" (buffer-string)))
        (should wime-edit--start)))))

(ert-deftest wime-tree-incomplete-syllable-keeps-text ()
  "One more key of a new syllable leaves the characters already shown alone."
  (wime-edit-test--buffer
    (let ((wime-candidate-style 'tree) (wime-tree-show-keys nil))
      (wime-edit-test--type "aa")
      (let ((blocks wime-tree--blocks))
        (wime-edit-test--type "b")
        (should (string-prefix-p "甲" (buffer-string)))
        (should (equal (mapcar #'wime-tree-block-text wime-tree--blocks)
                       (mapcar #'wime-tree-block-text blocks)))))))

(ert-deftest wime-tree-toggles-to-the-sentence-list ()
  (wime-edit-test--buffer
    (let ((wime-candidate-style 'tree))
      (wime-edit-test--type "aabb")
      (wime-toggle-candidate-list)
      (should-not (wime--spans-p)))))

(ert-deftest wime-raw-letters-stay-until-space ()
  "Raw input keeps the typed letters in the buffer; space converts them."
  (let ((wime-raw-input t))
    (wime-edit-test--buffer
      (wime-edit-test--type "aabb")
      (should (equal (buffer-string) "aabb"))
      (should-not (bound-and-true-p wime-edit-mode))
      (should (equal (mapcar #'wime-tree-block-text wime-tree--blocks) '("甲" "丙")))
      (wime-edit-test--type " ")
      (should (equal (buffer-string) "甲丙")))))

(ert-deftest wime-raw-digit-fixes-a-word ()
  (let ((wime-raw-input t))
    (wime-edit-test--buffer
      (wime-edit-test--type "aabb")
      (wime-edit-test--type "1")
      (should (equal (buffer-string) "aabb"))
      (wime-edit-test--type " ")
      (should (equal (buffer-string) "乙丙")))))

(ert-deftest wime-raw-deleting-every-letter-clears-the-tree ()
  (let ((wime-raw-input t))
    (wime-edit-test--buffer
      (wime-edit-test--type "aa")
      (should wime-raw--overlays)
      (wime-edit-test--command #'delete-char -1)
      (wime-edit-test--command #'delete-char -1)
      (should (equal (buffer-string) ""))
      (should-not wime-raw--overlays)
      (should-not wime-raw-mode))))

(defmacro wime-tree-test--words (&rest body)
  "Run BODY with 甲丙 as a word and eight characters for \"dd\"."
  (declare (indent 0))
  `(let ((wime-edit-test--dictionary
          (append '(("aabb" ("甲丙" "a/c" 100000))
                    ("dd" ("子" "a" 80) ("丑" "b" 70) ("寅" "c" 60) ("卯" "d" 50)
                     ("辰" "e" 40) ("巳" "f" 30) ("午" "g" 20) ("未" "h" 10)))
                  wime-edit-test--dictionary)))
     ,@body))

(ert-deftest wime-raw-cursor-in-a-word-lists-its-character-first ()
  "With the cursor on 好 in 你好, the alternatives start with characters for that
syllable; picking one changes 好 alone and keeps 你."
  (let ((wime-raw-input t))
    (wime-tree-test--words
      (wime-edit-test--buffer
        (wime-edit-test--type "aabb")
        (wime-edit-test--command #'backward-char 1)
        (should (equal (mapcar #'wime-tree-block-text wime-tree--blocks) '("甲丙")))
        (should (equal (seq-take (car wime--span-choices) 3) '(1 2 "丁")))
        (wime-edit-test--type "1")
        (wime-edit-test--command #'end-of-line)
        (wime-edit-test--type " ")
        (should (equal (buffer-string) "甲丁"))))))

(ert-deftest wime-raw-up-and-down-cycle-the-focused-word ()
  "Down replaces the focused word at once; up walks back to the original."
  (let ((wime-raw-input t))
    (wime-tree-test--words
      (wime-edit-test--buffer
        (wime-edit-test--type "aabb")
        (wime-edit-test--command #'backward-char 1)
        (wime-edit-test--command #'wime-raw-next-alternative)
        (should (equal (car wime--candidates) "甲丁"))
        (should (equal wime-tree--current 0))
        (should (equal (buffer-string) "aabb"))
        (wime-edit-test--command #'wime-raw-previous-alternative)
        (should (equal (car wime--candidates) "甲丙"))
        (wime-edit-test--command #'wime-raw-previous-alternative)
        (should (equal (car wime--candidates) "甲丙"))))))

(ert-deftest wime-raw-equals-pages-the-focused-word ()
  "= turns the focused word's alternatives to the next page; at the end of
the input it stays punctuation."
  (let ((wime-raw-input t) (wime-tree-focus-alts 3))
    (wime-tree-test--words
      (wime-edit-test--buffer
        (wime-edit-test--type "dd")
        (wime-edit-test--command #'backward-char 1)
        (let ((first (mapcar #'cl-third wime--span-choices)))
          (should (= (length first) 3))
          (wime-edit-test--type "=")
          (should (equal (buffer-string) "dd"))
          (should (= wime-tree--page 1))
          (should-not (equal (mapcar #'cl-third wime--span-choices) first))
          (wime-edit-test--type "-")
          (should (equal (mapcar #'cl-third wime--span-choices) first))
          (wime-edit-test--command #'end-of-line)
          (wime-edit-test--type ",")
          (should (equal (buffer-string) "子，")))))))

;;; wime-tree-test.el ends here
