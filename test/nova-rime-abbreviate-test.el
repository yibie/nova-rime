;;; nova-rime-abbreviate-test.el --- Initials typed alone, anywhere -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nova-rime)

(load (expand-file-name "fixture.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar nova-rime-abbreviate-test--database nil)

(defmacro nova-rime-abbreviate-test--with (&rest body)
  "Run BODY on the fixture dictionary with `nova-rime-abbreviate' on."
  (declare (indent 0))
  `(let ((nova-rime-database-file (or nova-rime-abbreviate-test--database
                                 (setq nova-rime-abbreviate-test--database (nova-rime-test--fixture))))
         (nova-rime--db nil) (nova-rime--db-failed nil) (nova-rime--initials-index nil)
         (nova-rime--abbreviation-state nil) (nova-rime--search-cache nil)
         (nova-rime-abbreviate t) (nova-rime-word-penalty 20.0) (nova-rime-abbreviation-penalty 4.0)
         (nova-rime-user-database nil) (nova-rime-grammar-module nil))
     (clrhash nova-rime--abbreviation-cache)
     (unwind-protect (progn ,@body)
       (when nova-rime--db (sqlite-close nova-rime--db)))))

(defun nova-rime-abbreviate-test--codes (input)
  (mapcar #'car (nova-rime--parse input)))

(ert-deftest nova-rime-abbreviate-full-syllables-stay-paired ()
  "Keys that spell words as full syllables keep pairing up."
  (nova-rime-abbreviate-test--with
    (should (equal (nova-rime-abbreviate-test--codes "nihk") '("ni" "hk")))
    (should (equal (nova-rime-abbreviate-test--codes "womfm;tm") '("wo" "mf" "m;" "tm")))))

(ert-deftest nova-rime-abbreviate-initial-in-the-middle ()
  "An initial alone mid-sentence splits off and decodes to a word."
  (nova-rime-abbreviate-test--with
    ;; 我们 明天: 们 typed as \"m\" alone.
    (should (equal (nova-rime-abbreviate-test--codes "womm;tm") '("wo" "m" "m;" "tm")))
    (let ((nova-rime--input "womm;tm"))
      (should (equal (car (nova-rime--candidates "womm;tm")) "我们明天")))))

(ert-deftest nova-rime-abbreviate-all-initials ()
  "A word typed as initials only decodes as that word."
  (nova-rime-abbreviate-test--with
    (should (equal (nova-rime-abbreviate-test--codes "nh") '("n" "h")))
    (let ((nova-rime--input "nh"))
      (should (equal (car (nova-rime--candidates "nh")) "你好")))))

(ert-deftest nova-rime-abbreviate-trailing-initial ()
  "A trailing initial is a syllable, so the reading covers every key."
  (nova-rime-abbreviate-test--with
    (let ((nova-rime--input "nihkm"))
      (should (equal (nova-rime-abbreviate-test--codes "nihkm") '("ni" "hk" "m")))
      (should (= (length (car (nova-rime--candidates "nihkm"))) 3)))))

(ert-deftest nova-rime-abbreviate-off-keeps-pairs ()
  "With `nova-rime-abbreviate' off, keys always pair up."
  (nova-rime-abbreviate-test--with
    (let ((nova-rime-abbreviate nil))
      (should (equal (nova-rime-abbreviate-test--codes "womm;tm") '("wo" "mm" ";t" "m"))))))

;;; nova-rime-abbreviate-test.el ends here
