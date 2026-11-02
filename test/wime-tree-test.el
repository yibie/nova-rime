;;; wime-tree-test.el --- Tree candidate view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'wime-edit)
(require 'wime-tree)

(ert-deftest wime-tree-segments-keep-differences-whole ()
  (let ((wime-candidates-max 4))
    (should (equal (wime-tree--segments "你好我很喜欢"
                                        '("拟好我很喜欢" "泥蒿我很喜欢" "你好握很喜欢")
                                        '(2 3 4 6))
                   '((0 2 "拟好" "泥蒿") (2 3 "握") (3 4) (4 6))))))

(ert-deftest wime-tree-drops-readings-crossing-a-boundary ()
  (let ((wime-candidates-max 4))
    (should (equal (wime-tree--segments "我很喜欢" '("沃狠喜欢" "握很喜欢") '(1 2 4))
                   '((0 1 "握") (1 2) (2 4))))))

;; `wime-edit-test--buffer' comes from wime-edit-test.el, loaded first by run.el.
(ert-deftest wime-tree-shows-and-digit-fixes-a-chip ()
  "The tree needs no reranker; a digit fixes one chip and keeps composing."
  (wime-edit-test--buffer
    (let ((wime-candidate-style 'tree))
      (wime-edit-test--type "aabb")
      (should (wime--spans-p))
      (let ((tree (cl-some (lambda (overlay) (overlay-get overlay 'after-string))
                           wime-edit--correction-decorations)))
        (should (string-search " 甲 " tree))
        (should (string-search " 丙 " tree))
        (should (string-search "╰1" tree)))
      (let ((choice (car wime--span-choices)))
        (should choice)
        (wime-edit-test--type "1")
        (should (string-search (nth 2 choice) (buffer-string)))
        (should wime-edit--start)))))

(ert-deftest wime-tree-toggles-to-the-sentence-list ()
  (wime-edit-test--buffer
    (let ((wime-candidate-style 'tree))
      (wime-edit-test--type "aabb")
      (wime-toggle-candidate-list)
      (should-not (wime--spans-p)))))

;;; wime-tree-test.el ends here
