;;; run.el --- Standalone runtime tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)

(unless (sqlite-available-p)
  (error "Nova-rime tests require Emacs built with SQLite support"))

(let* ((directory (file-name-directory (or load-file-name buffer-file-name)))
       (package-directory (expand-file-name ".." directory)))
  (add-to-list 'load-path package-directory)
  (setq nova-rime-user-database nil
        nova-rime-grammar-module nil
        native-comp-enable-subr-trampolines nil)
  (dolist (file '("nova-rime-test.el" "nova-rime-edit-test.el" "nova-rime-tree-test.el" "nova-rime-abbreviate-test.el"
                  "nova-rime-memory-test.el" "schemes-test.el"))
    (load (expand-file-name file directory) nil t)))

(ert-run-tests-batch-and-exit)
