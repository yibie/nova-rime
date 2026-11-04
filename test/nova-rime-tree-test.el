;;; nova-rime-tree-test.el --- Tree candidates and raw-letter input -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nova-rime-edit)
(require 'nova-rime-tree)
(require 'nova-rime-raw)

;; `nova-rime-edit-test--buffer' and its dictionary come from nova-rime-edit-test.el,
;; loaded first by run.el.  甲 and 乙 share "aa" at 100 and 90, so 甲 is an
;; uncertain word and lists 乙 with a sentence-wide label.

(defun nova-rime-tree-test--tree ()
  (cl-some (lambda (overlay) (overlay-get overlay 'after-string))
           nova-rime-edit--correction-decorations))

(ert-deftest nova-rime-tree-shows-chips-and-digit-fixes-one ()
  "The tree needs no reranker; a digit fixes one chip and keeps composing."
  (nova-rime-edit-test--buffer
    (let ((nova-rime-candidate-style 'tree) (nova-rime-tree-show-keys nil))
      (nova-rime-edit-test--type "aabb")
      (should (nova-rime--spans-p))
      (let ((tree (nova-rime-tree-test--tree)))
        (should (string-search " 甲 " tree))
        (should (string-search " 丙 " tree))
        (should (string-search "╰1" tree)))
      (let ((choice (car nova-rime--span-choices)))
        (should (equal (nth 2 choice) "乙"))
        (nova-rime-edit-test--type "1")
        (should (string-search "乙" (buffer-string)))
        (should nova-rime-edit--start)))))

(ert-deftest nova-rime-tree-incomplete-syllable-keeps-text ()
  "One more key of a new syllable leaves the characters already shown alone."
  (nova-rime-edit-test--buffer
    (let ((nova-rime-candidate-style 'tree) (nova-rime-tree-show-keys nil))
      (nova-rime-edit-test--type "aa")
      (let ((blocks nova-rime-tree--blocks))
        (nova-rime-edit-test--type "b")
        (should (string-prefix-p "甲" (buffer-string)))
        (should (equal (mapcar #'nova-rime-tree-block-text nova-rime-tree--blocks)
                       (mapcar #'nova-rime-tree-block-text blocks)))))))

(ert-deftest nova-rime-tree-toggles-to-the-sentence-list ()
  (nova-rime-edit-test--buffer
    (let ((nova-rime-candidate-style 'tree))
      (nova-rime-edit-test--type "aabb")
      (nova-rime-toggle-candidate-list)
      (should-not (nova-rime--spans-p)))))

(ert-deftest nova-rime-raw-letters-stay-until-space ()
  "Raw input keeps the typed letters in the buffer; space converts them."
  (let ((nova-rime-raw-input t))
    (nova-rime-edit-test--buffer
      (nova-rime-edit-test--type "aabb")
      (should (equal (buffer-string) "aabb"))
      (should-not (bound-and-true-p nova-rime-edit-mode))
      (should (equal (mapcar #'nova-rime-tree-block-text nova-rime-tree--blocks) '("甲" "丙")))
      (nova-rime-edit-test--type " ")
      (should (equal (buffer-string) "甲丙")))))

(ert-deftest nova-rime-raw-digit-fixes-a-word ()
  (let ((nova-rime-raw-input t))
    (nova-rime-edit-test--buffer
      (nova-rime-edit-test--type "aabb")
      (nova-rime-edit-test--type "1")
      (should (equal (buffer-string) "aabb"))
      (nova-rime-edit-test--type " ")
      (should (equal (buffer-string) "乙丙")))))

(ert-deftest nova-rime-raw-deleting-every-letter-clears-the-tree ()
  (let ((nova-rime-raw-input t))
    (nova-rime-edit-test--buffer
      (nova-rime-edit-test--type "aa")
      (should nova-rime-raw--overlays)
      (nova-rime-edit-test--command #'delete-char -1)
      (nova-rime-edit-test--command #'delete-char -1)
      (should (equal (buffer-string) ""))
      (should-not nova-rime-raw--overlays)
      (should-not nova-rime-raw-mode))))

(defmacro nova-rime-tree-test--words (&rest body)
  "Run BODY with 甲丙 as a word and eight characters for \"dd\"."
  (declare (indent 0))
  `(let ((nova-rime-edit-test--dictionary
          (append '(("aabb" ("甲丙" "a/c" 100000))
                    ("dd" ("子" "a" 80) ("丑" "b" 70) ("寅" "c" 60) ("卯" "d" 50)
                     ("辰" "e" 40) ("巳" "f" 30) ("午" "g" 20) ("未" "h" 10)))
                  nova-rime-edit-test--dictionary)))
     ,@body))

(ert-deftest nova-rime-raw-cursor-in-a-word-lists-its-character-first ()
  "With the cursor on 好 in 你好, the alternatives start with characters for that
syllable; picking one changes 好 alone and keeps 你."
  (let ((nova-rime-raw-input t))
    (nova-rime-tree-test--words
      (nova-rime-edit-test--buffer
        (nova-rime-edit-test--type "aabb")
        (nova-rime-edit-test--command #'backward-char 1)
        (should (equal (mapcar #'nova-rime-tree-block-text nova-rime-tree--blocks) '("甲丙")))
        (should (equal (seq-take (car nova-rime--span-choices) 3) '(1 2 "丁")))
        (nova-rime-edit-test--type "1")
        (nova-rime-edit-test--command #'end-of-line)
        (nova-rime-edit-test--type " ")
        (should (equal (buffer-string) "甲丁"))))))

(ert-deftest nova-rime-raw-fixing-a-word-regroups-the-words-after-it ()
  "Picking 乙 for the first syllable lets 丙戊 after it become 乙丁己:
freezing only holds the words before the choice."
  (let ((nova-rime-raw-input t)
        (nova-rime-edit-test--dictionary
         (append '(("bbcc" ("丙戊" "c/e" 1000000))
                   ("aabbcc" ("乙丁己" "b/d/f" 95000000)))
                 nova-rime-edit-test--dictionary)))
    (nova-rime-edit-test--buffer
      (nova-rime-edit-test--type "aabbcc")
      (should (equal (mapcar #'nova-rime-tree-block-text nova-rime-tree--blocks) '("甲" "丙戊")))
      (nova-rime-raw--apply (list 0 1 "乙"))
      (should (equal (car nova-rime--candidates) "乙丁己"))
      (nova-rime-edit-test--type " ")
      (should (equal (buffer-string) "乙丁己")))))

(ert-deftest nova-rime-raw-fixing-a-word-keeps-the-words-before-it ()
  "Picking 丁 for the last syllable leaves 甲 before it alone, even though
乙丁 would score higher as a whole."
  (let ((nova-rime-raw-input t)
        (nova-rime-edit-test--dictionary
         (append '(("aabb" ("乙丁" "b/d" 9500)))
                 nova-rime-edit-test--dictionary)))
    (nova-rime-edit-test--buffer
      (nova-rime-edit-test--type "aabb")
      (should (equal (car nova-rime--candidates) "甲丙"))
      (nova-rime-raw--apply (list 1 2 "丁"))
      (should (equal (car nova-rime--candidates) "甲丁")))))

(ert-deftest nova-rime-raw-up-and-down-cycle-the-focused-word ()
  "Down replaces the focused word at once; up walks back to the original."
  (let ((nova-rime-raw-input t))
    (nova-rime-tree-test--words
      (nova-rime-edit-test--buffer
        (nova-rime-edit-test--type "aabb")
        (nova-rime-edit-test--command #'backward-char 1)
        (nova-rime-edit-test--command #'nova-rime-raw-next-alternative)
        (should (equal (car nova-rime--candidates) "甲丁"))
        (should (equal nova-rime-tree--current 0))
        (should (equal (buffer-string) "aabb"))
        (nova-rime-edit-test--command #'nova-rime-raw-previous-alternative)
        (should (equal (car nova-rime--candidates) "甲丙"))
        (nova-rime-edit-test--command #'nova-rime-raw-previous-alternative)
        (should (equal (car nova-rime--candidates) "甲丙"))))))

(ert-deftest nova-rime-raw-equals-pages-the-focused-word ()
  "= turns the focused word's alternatives to the next page; at the end of
the input it stays punctuation."
  (let ((nova-rime-raw-input t) (nova-rime-tree-focus-alts 3))
    (nova-rime-tree-test--words
      (nova-rime-edit-test--buffer
        (nova-rime-edit-test--type "dd")
        (nova-rime-edit-test--command #'backward-char 1)
        (let ((first (mapcar #'cl-third nova-rime--span-choices)))
          (should (= (length first) 3))
          (nova-rime-edit-test--type "=")
          (should (equal (buffer-string) "dd"))
          (should (= nova-rime-tree--page 1))
          (should-not (equal (mapcar #'cl-third nova-rime--span-choices) first))
          (nova-rime-edit-test--type "-")
          (should (equal (mapcar #'cl-third nova-rime--span-choices) first))
          (nova-rime-edit-test--command #'end-of-line)
          (nova-rime-edit-test--type ",")
          (should (equal (buffer-string) "子，")))))))

;;; nova-rime-tree-test.el ends here
