;;; wime-abbreviate-test.el --- Initials typed alone, anywhere -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'wime)

(load (expand-file-name "fixture.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar wime-abbreviate-test--database nil)

(defmacro wime-abbreviate-test--with (&rest body)
  "Run BODY on the fixture dictionary with `wime-abbreviate' on."
  (declare (indent 0))
  `(let ((wime-database-file (or wime-abbreviate-test--database
                                 (setq wime-abbreviate-test--database (wime-test--fixture))))
         (wime--db nil) (wime--db-failed nil) (wime--initials-index nil)
         (wime--abbreviation-state nil) (wime--search-cache nil)
         (wime-abbreviate t) (wime-word-penalty 20.0) (wime-abbreviation-penalty 4.0)
         (wime-user-database nil) (wime-grammar-module nil) (wime-decoding 'lattice))
     (clrhash wime--abbreviation-cache)
     (unwind-protect (progn ,@body)
       (when wime--db (sqlite-close wime--db)))))

(defun wime-abbreviate-test--codes (input)
  (mapcar #'car (wime--parse input)))

(ert-deftest wime-abbreviate-full-syllables-stay-paired ()
  "Keys that spell words as full syllables keep pairing up."
  (wime-abbreviate-test--with
    (should (equal (wime-abbreviate-test--codes "nihk") '("ni" "hk")))
    (should (equal (wime-abbreviate-test--codes "womfm;tm") '("wo" "mf" "m;" "tm")))))

(ert-deftest wime-abbreviate-initial-in-the-middle ()
  "An initial alone mid-sentence splits off and decodes to a word."
  (wime-abbreviate-test--with
    ;; 我们 明天: 们 typed as \"m\" alone.
    (should (equal (wime-abbreviate-test--codes "womm;tm") '("wo" "m" "m;" "tm")))
    (let ((wime--input "womm;tm"))
      (should (equal (car (wime--candidates "womm;tm")) "我们明天")))))

(ert-deftest wime-abbreviate-all-initials ()
  "A word typed as initials only decodes as that word."
  (wime-abbreviate-test--with
    (should (equal (wime-abbreviate-test--codes "nh") '("n" "h")))
    (let ((wime--input "nh"))
      (should (equal (car (wime--candidates "nh")) "你好")))))

(ert-deftest wime-abbreviate-trailing-initial ()
  "A trailing initial is a syllable, so the reading covers every key."
  (wime-abbreviate-test--with
    (let ((wime--input "nihkm"))
      (should (equal (wime-abbreviate-test--codes "nihkm") '("ni" "hk" "m")))
      (should (= (length (car (wime--candidates "nihkm"))) 3)))))

(ert-deftest wime-abbreviate-off-keeps-pairs ()
  "With `wime-abbreviate' off, keys always pair up."
  (wime-abbreviate-test--with
    (let ((wime-abbreviate nil))
      (should (equal (wime-abbreviate-test--codes "womm;tm") '("wo" "mm" ";t" "m"))))))

;;; wime-abbreviate-test.el ends here
