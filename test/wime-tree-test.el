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

;;; wime-tree-test.el ends here
