;;; nova-rime-memory.el --- Manage Nova-rime learned readings and history -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Manage the live input method's user database.  Every mutation checks the
;; displayed row, runs in a transaction, and returns an inverse-operation
;; record.  History and learned preferences have independent lifetimes.

;;; Code:

(require 'nova-rime)
(require 'tabulated-list)
(require 'thingatpt)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(cl-defstruct (nova-rime-memory-change (:constructor nova-rime-memory--make-change))
  database kind before after)

(defun nova-rime-memory--database ()
  "Return the configured database, or explain how to enable learning."
  (or (nova-rime--user-db)
      (user-error "尚未启用学习，请先设置 nova-rime-user-database")))

(defun nova-rime-memory--file ()
  (and nova-rime-user-database (expand-file-name nova-rime-user-database)))

(defun nova-rime-memory-rows (kind &optional query limit within)
  "Return KIND's newest rows matching literal QUERY, up to LIMIT.
KIND is `memory' or `history'.  Memory rows are (CODE TEXT AUX COUNT UPDATED);
history rows are (ID TIME INPUT KEYS TEXT DEFAULT CONTEXT).
With WITHIN, also find learned words contained in QUERY."
  (let ((db (nova-rime-memory--database)) (query (or query ""))
        (limit (or limit 200)))
    (unless (and (integerp limit) (> limit 0)) (error "Invalid row limit"))
    (pcase kind
      ('memory
       (sqlite-select db
                      (concat "SELECT code, text, aux, count, updated FROM memory "
                              "WHERE instr(code, ?1) > 0 OR instr(text, ?1) > 0 "
                              "OR (?2 AND instr(?1, text) > 0) "
                              "ORDER BY updated DESC, code, text LIMIT ?3")
                      (list query (if within 1 0) limit)))
      ('history
       (sqlite-select db
                      (concat "SELECT rowid, time, input, keys, text, default_reading, context FROM commits "
                              "WHERE instr(input, ?1) > 0 OR instr(keys, ?1) > 0 OR instr(text, ?1) > 0 "
                              "OR instr(default_reading, ?1) > 0 OR instr(context, ?1) > 0 "
                              "ORDER BY time DESC, rowid DESC LIMIT ?2")
                      (list query limit)))
      (_ (error "Unknown record kind: %S" kind)))))

(defun nova-rime-memory--id (kind row)
  (if (eq kind 'memory) (seq-take row 2) (car row)))

(defun nova-rime-memory--row (db kind row)
  "Read the current record with ROW's identity."
  (car (pcase kind
         ('memory
          (sqlite-select db "SELECT code, text, aux, count, updated FROM memory WHERE code = ? AND text = ?"
                         (seq-take row 2)))
         ('history
          (sqlite-select db "SELECT rowid, time, input, keys, text, default_reading, context FROM commits WHERE rowid = ?"
                         (list (car row))))
         (_ (error "Unknown record kind: %S" kind)))))

(defun nova-rime-memory--refresh-input ()
  "Refresh live Chinese compositions after changing their preferences."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (equal current-input-method "nova-rime") nova-rime-mode (nova-rime--active-p))
        (setq nova-rime--search-cache nil)
        (condition-case err
            (nova-rime--refresh)
          (error (message "学习记录已保存；%s 的候选刷新失败：%s"
                          (buffer-name) (error-message-string err))))))))

(defun nova-rime-memory--change (kind before after)
  "Atomically replace exact BEFORE rows with AFTER rows and return a change.
Reject stale records and occupied destinations, including during undo."
  (let ((db (nova-rime-memory--database)) committed)
    (sqlite-execute db "BEGIN IMMEDIATE")
    (unwind-protect
        (progn
          (dolist (row before)
            (unless (equal row (nova-rime-memory--row db kind row))
              (user-error "记录已发生变化，请刷新后重试")))
          (dolist (row before)
            (pcase kind
              ('memory (sqlite-execute db "DELETE FROM memory WHERE code = ? AND text = ?"
                                       (seq-take row 2)))
              ('history (sqlite-execute db "DELETE FROM commits WHERE rowid = ?" (list (car row))))))
          (dolist (row after)
            (when (nova-rime-memory--row db kind row)
              (user-error "目标记录已存在，操作未覆盖现有数据"))
            (pcase kind
              ('memory (sqlite-execute db "INSERT INTO memory (code, text, aux, count, updated) VALUES (?, ?, ?, ?, ?)" row))
              ('history (sqlite-execute db "INSERT INTO commits (rowid, time, input, keys, text, default_reading, context) VALUES (?, ?, ?, ?, ?, ?, ?)" row))))
          (sqlite-execute db "COMMIT")
          (setq committed t))
      (unless committed (sqlite-execute db "ROLLBACK")))
    (when (eq kind 'memory)
      (nova-rime--reload-memory db)
      (nova-rime-memory--refresh-input))
    (nova-rime-memory--make-change :database (nova-rime-memory--file) :kind kind
                              :before (copy-tree before) :after (copy-tree after))))

(defun nova-rime-memory-delete-records (kind rows)
  "Delete exact ROWS of KIND, returning a reversible change."
  (unless rows (user-error "没有选中的记录"))
  (nova-rime-memory--change kind (delete-dups (copy-tree rows)) nil))

(defun nova-rime-memory-revert-change (change)
  "Undo CHANGE without overwriting records learned or edited since it."
  (unless (equal (nova-rime-memory-change-database change) (nova-rime-memory--file))
    (user-error "学习数据库已切换，请回到原数据库后撤销"))
  (nova-rime-memory--change (nova-rime-memory-change-kind change)
                       (nova-rime-memory-change-after change) (nova-rime-memory-change-before change)))

(defun nova-rime-memory--validated-aux (code text)
  "Validate Chinese TEXT's complete double-pinyin CODE; derive auxiliary keys."
  (let ((units (nova-rime--parse code)) (db (nova-rime--database)))
    (unless (and (<= 2 (length text) 16) (= (length units) (length text))
                 (string-match-p "\\`[㐀-鿿]+\\'" text)
                 (equal code (nova-rime--full-code units))
                 (cl-every (lambda (unit) (and (= (length (car unit)) 2)
                                               (string-empty-p (cdr unit)))) units))
      (user-error "请输入 2–16 个汉字及其完整双拼编码（不含辅助码）"))
    (unless db (user-error "当前词库不可用，无法校验读音"))
    (cl-flet ((aux (code word)
                (caar (sqlite-select db "SELECT aux FROM entry WHERE code = ? AND word = ? ORDER BY weight DESC LIMIT 1"
                                     (list code word)))))
      (or (aux code text)
          (mapconcat (lambda (i)
                       (let ((code (car (nth i units))) (char (substring text i (1+ i))))
                         (or (aux code char) (user-error "词库中没有 %s 对应 %s 的读音" code char))))
                     (number-sequence 0 (1- (length text))) "/")))))

(defun nova-rime-memory-update-record (row code text)
  "Replace learned ROW with validated CODE/TEXT and newly derived auxiliary keys.
A changed reading starts a new learning count.  Undo restores the old count."
  (when (equal (list code text) (seq-take row 2))
    (user-error "词语和编码没有变化"))
  (nova-rime-memory--change 'memory (list row)
                       (list (list code text (nova-rime-memory--validated-aux code text) 1 (float-time)))))

;;; Management buffer

(defcustom nova-rime-memory-page-size 200
  "Number of records loaded initially and by `nova-rime-memory-more'."
  :type 'natnum :group 'nova-rime)

(defvar-local nova-rime-memory--kind 'memory)
(defvar-local nova-rime-memory--query "")
(defvar-local nova-rime-memory--within nil)
(defvar-local nova-rime-memory--limit 200)
(defvar-local nova-rime-memory--rows nil)
(defvar-local nova-rime-memory--database-file nil)
(defvar-local nova-rime-memory--dictionary-file nil)
(defvar-local nova-rime-memory--undo nil)

(defvar nova-rime-memory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (dolist (binding '(("RET" . nova-rime-memory-details) ("e" . nova-rime-memory-edit)
                       ("m" . nova-rime-memory-mark) ("u" . nova-rime-memory-unmark)
                       ("d" . nova-rime-memory-delete) ("U" . nova-rime-memory-undo)
                       ("C-/" . nova-rime-memory-undo) ("/" . nova-rime-memory-filter)
                       ("g" . nova-rime-memory-refresh) ("+" . nova-rime-memory-more) ("h" . nova-rime-history)
                       ("l" . nova-rime-memory) ("?" . nova-rime-memory-help)))
      (define-key map (kbd (car binding)) (cdr binding)))
    map))

(define-derived-mode nova-rime-memory-mode tabulated-list-mode "Nova-rime 学习"
  "Manage learned readings and commit history.
Use `nova-rime-memory-help' for commands and their effects."
  (setq-local tabulated-list-padding 2)
  (setq-local revert-buffer-function (lambda (&rest _) (nova-rime-memory-refresh))))

;; Keep Evil optional and support either package load order.  Preserve its
;; movement, search and column-navigation keys; use separate view commands.
(with-eval-after-load 'evil
  (evil-set-initial-state 'nova-rime-memory-mode 'normal)
  (evil-define-key* '(normal motion) nova-rime-memory-mode-map
    (kbd "RET") #'nova-rime-memory-details
    "e" #'nova-rime-memory-edit
    "m" #'nova-rime-memory-mark
    "M" #'nova-rime-memory-unmark
    "d" #'nova-rime-memory-delete
    "u" #'nova-rime-memory-undo
    "U" #'nova-rime-memory-undo
    (kbd "C-/") #'nova-rime-memory-undo
    "s" #'nova-rime-memory-filter
    "+" #'nova-rime-memory-more
    "H" #'nova-rime-history
    "L" #'nova-rime-memory
    "gr" #'nova-rime-memory-refresh
    "g?" #'nova-rime-memory-help
    "q" #'quit-window))

(defun nova-rime-memory--check-database (&optional dictionary)
  (unless (equal nova-rime-memory--database-file (nova-rime-memory--file))
    (user-error "学习数据库已切换，请重新打开 M-x nova-rime-memory"))
  (when (and dictionary (not (equal nova-rime-memory--dictionary-file nova-rime-database-file)))
    (user-error "输入方案已切换，请重新打开列表后修改读音")))

(defun nova-rime-memory--time (time)
  (if (numberp time) (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time time)) "—"))

(defun nova-rime-memory--cell (text)
  "Render stored text on one list row, without interpreted properties."
  (replace-regexp-in-string "[[:cntrl:]]" " " (substring-no-properties (or text ""))))

(defun nova-rime-memory--marked-ids ()
  (save-excursion
    (goto-char (point-min))
    (let (ids)
      (while (not (eobp))
        (when (eq (char-after) ?*) (push (tabulated-list-get-id) ids))
        (forward-line 1))
      (nreverse ids))))

(defun nova-rime-memory-refresh ()
  "Reload records, retaining visible selection marks."
  (interactive)
  (nova-rime-memory--check-database)
  (let* ((marked (nova-rime-memory--marked-ids))
         (rows (nova-rime-memory-rows nova-rime-memory--kind nova-rime-memory--query
                                 (1+ nova-rime-memory--limit) nova-rime-memory--within))
         (more (> (length rows) nova-rime-memory--limit)))
    (setq nova-rime-memory--rows
          (mapcar (lambda (row) (cons (nova-rime-memory--id nova-rime-memory--kind row) row))
                  (seq-take rows nova-rime-memory--limit))
          tabulated-list-entries
          (mapcar
           (lambda (entry)
             (let ((row (cdr entry)))
               (list (car entry)
                     (if (eq nova-rime-memory--kind 'memory)
                         (vector (nova-rime-memory--cell (nth 1 row)) (nova-rime-memory--cell (nth 0 row))
                                 (number-to-string (or (nth 3 row) 0))
                                 (nova-rime-memory--time (nth 4 row)) (nova-rime-memory--cell (nth 2 row)))
                       (vector (nova-rime-memory--time (nth 1 row)) (nova-rime-memory--cell (nth 4 row))
                               (nova-rime-memory--cell (nth 5 row)) (nova-rime-memory--cell (nth 2 row))
                               (nova-rime-memory--cell (nth 6 row)))))))
           nova-rime-memory--rows)
          mode-line-process (format " [%d%s%s]" (length tabulated-list-entries)
                                    (if more "+" "")
                                    (if (string-empty-p nova-rime-memory--query) "" " 筛选")))
    (tabulated-list-print t)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (member (tabulated-list-get-id) marked) (tabulated-list-put-tag "*"))
        (forward-line 1)))
    (message (substitute-command-keys "%s：%d 条%s · \\[nova-rime-memory-filter] 筛选 · \\[nova-rime-memory-details] 详情 · \\[nova-rime-memory-mark]/\\[nova-rime-memory-unmark] 标记 · \\[nova-rime-memory-delete] 删除 · \\[nova-rime-memory-undo] 撤销 · \\[nova-rime-memory-help] 帮助")
             (if (eq nova-rime-memory--kind 'memory) "学习偏好" "上屏历史")
             (length tabulated-list-entries) (if more "（+ 加载更多）" ""))))

(defun nova-rime-memory--open (kind query &optional within)
  (nova-rime-memory--database)
  (let ((file (nova-rime-memory--file))
        (buffer (get-buffer-create "*Nova-rime 学习*")))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'nova-rime-memory-mode)
                   (equal nova-rime-memory--database-file file))
        (nova-rime-memory-mode)
        (setq nova-rime-memory--database-file file))
      (setq nova-rime-memory--kind kind nova-rime-memory--query (or query "")
            nova-rime-memory--within within nova-rime-memory--dictionary-file nova-rime-database-file
            nova-rime-memory--limit (max 1 nova-rime-memory-page-size)
            list-buffers-directory file tabulated-list-sort-key nil
            mode-name (if (eq kind 'memory) "Nova-rime 学习偏好" "Nova-rime 上屏历史")
            tabulated-list-format
            (if (eq kind 'memory)
                [("词语" 24 t) ("双拼编码" 22 t)
                 ("学习次数" 8 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                                                (string-to-number (aref (cadr b) 2)))))
                 ("最近学习" 19 t) ("辅助码" 0 t)]
              [("上屏时间" 19 t) ("最终文本" 26 t) ("最初候选" 26 t)
               ("原始按键" 22 t) ("前文" 0 t)]))
      ;; Marks belong to one query and view; changing either clears them.
      (let ((inhibit-read-only t)) (erase-buffer))
      (tabulated-list-init-header)
      (nova-rime-memory-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun nova-rime-memory (&optional query)
  "Manage learned Chinese readings, optionally filtered by QUERY."
  (interactive)
  (nova-rime-memory--open 'memory query))

;;;###autoload
(defun nova-rime-history (&optional query)
  "Browse commit history, optionally filtered by QUERY.
Deleting history leaves learned preferences intact."
  (interactive)
  (nova-rime-memory--open 'history query))

;;;###autoload
(defun nova-rime-memory-at-point ()
  "Find learned words in the region, current candidate, or word at point."
  (interactive)
  (let ((text (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                    ((and (equal current-input-method "nova-rime") (nova-rime--active-p)) (car nova-rime--candidates))
                    (t (thing-at-point 'word t)))))
    (unless (and text (not (string-empty-p text)))
      (user-error "请选中要查找的词语，或把光标放在词语上"))
    (nova-rime-memory--open 'memory text t)))

(defun nova-rime-memory-filter (query)
  "Filter the current view by literal QUERY; empty QUERY shows all rows."
  (interactive (list (read-string "筛选文字或编码（空白显示全部）：" nova-rime-memory--query)))
  (nova-rime-memory--check-database)
  (nova-rime-memory--open nova-rime-memory--kind query))

(defun nova-rime-memory-more ()
  "Load the next page without losing selection marks."
  (interactive)
  (cl-incf nova-rime-memory--limit (max 1 nova-rime-memory-page-size))
  (nova-rime-memory-refresh))

(defun nova-rime-memory--selected-row ()
  (or (cdr (assoc (tabulated-list-get-id) nova-rime-memory--rows))
      (user-error "当前行没有记录")))

(defun nova-rime-memory-mark ()
  "Mark this row for deletion and move down."
  (interactive)
  (nova-rime-memory--selected-row)
  (tabulated-list-put-tag "*" t))

(defun nova-rime-memory-unmark ()
  "Unmark this row and move down."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun nova-rime-memory-delete ()
  "Delete marked rows, or this row when none are marked.
Use `nova-rime-memory-undo' to restore the deleted records."
  (interactive)
  (nova-rime-memory--check-database)
  (let* ((marked (nova-rime-memory--marked-ids))
         (rows (if marked (mapcar (lambda (id) (cdr (assoc id nova-rime-memory--rows))) marked)
                 (list (nova-rime-memory--selected-row)))))
    (push (nova-rime-memory-delete-records nova-rime-memory--kind rows) nova-rime-memory--undo)
    (nova-rime-memory-refresh)
    (message (substitute-command-keys "已删除 %d 条%s；\\[nova-rime-memory-undo] 撤销%s") (length rows)
             (if (eq nova-rime-memory--kind 'memory) "学习偏好" "上屏历史")
             (if (eq nova-rime-memory--kind 'history) "（学习偏好保留）" "（上屏历史保留）"))))

(defun nova-rime-memory-edit ()
  "Edit the current learned word and its reading, validating against the dictionary."
  (interactive)
  (nova-rime-memory--check-database t)
  (unless (eq nova-rime-memory--kind 'memory) (user-error "上屏历史保留原始输入；请在学习偏好视图修改词语"))
  (let* ((row (nova-rime-memory--selected-row))
         (text (read-string "词语：" (nth 1 row)))
         (code (read-string "完整双拼编码：" (nth 0 row))))
    (nova-rime-memory--check-database t)
    (push (nova-rime-memory-update-record row code text) nova-rime-memory--undo)
    (nova-rime-memory-refresh)
    (message (substitute-command-keys "已修改学习偏好并重新校验辅助码；\\[nova-rime-memory-undo] 撤销"))))

(defun nova-rime-memory-undo ()
  "Undo the last successful management action in this buffer."
  (interactive)
  (nova-rime-memory--check-database)
  (unless nova-rime-memory--undo (user-error "没有可撤销的管理操作"))
  (nova-rime-memory-revert-change (car nova-rime-memory--undo))
  (pop nova-rime-memory--undo)
  (nova-rime-memory-refresh)
  (message "已撤销上一次管理操作"))

(defun nova-rime-memory-details ()
  "Show the complete record, including unabridged input and context."
  (interactive)
  (let ((row (nova-rime-memory--selected-row)) (kind nova-rime-memory--kind)
        (file nova-rime-memory--database-file))
    (with-help-window "*Nova-rime 记录详情*"
      (princ (if (eq kind 'memory)
                 (format "学习偏好\n\n词语：%s\n双拼编码：%s\n辅助码：%s\n学习次数：%s\n最近学习：%s\n\n删除偏好会让输入法忘记它，上屏历史仍保留。\n"
                         (nth 1 row) (nth 0 row) (nth 2 row) (nth 3 row) (nova-rime-memory--time (nth 4 row)))
               (format "上屏历史 · %s\n\n原始按键：%s\n完整编码：%s\n\n最初候选：\n%s\n\n最终文本：\n%s\n\n前文：\n%s\n\n删除这条历史不会删除已经学到的偏好。\n"
                       (nova-rime-memory--time (nth 1 row)) (nth 2 row) (nth 3 row)
                       (nth 5 row) (nth 4 row) (or (nth 6 row) ""))))
      (princ (format "\n数据文件：%s\n" file)))))

(defun nova-rime-memory-help ()
  "Show the management commands using the current key bindings."
  (interactive)
  ;; Resolve keys in the manager before entering the help buffer.
  (let ((text
         (substitute-command-keys
          "Nova-rime 学习管理\n\n \\[nova-rime-memory]  学习偏好       \\[nova-rime-history]  上屏历史\n \\[nova-rime-memory-filter]  筛选文字或编码   \\[nova-rime-memory-refresh]  刷新   \\[nova-rime-memory-more]  加载更多\n \\[nova-rime-memory-details]  完整详情   \\[nova-rime-memory-edit]  修改学习词语及编码\n \\[nova-rime-memory-mark]  标记   \\[nova-rime-memory-unmark]  取消标记\n \\[nova-rime-memory-delete]  删除标记记录；没有标记时删除当前行\n \\[nova-rime-memory-undo]  撤销最近一次修改或删除\n \\[quit-window]  返回\n\nEvil 普通状态保留 h/j/k/l、gg/G 和 /、?、n/N 导航与搜索。\n修改与删除可在这个管理 buffer 存活期间撤销。\n学习偏好影响候选；上屏历史保存当时的输入，两者分别管理。\n当前管理的是中文学习记录；修改读音会按当前词库校验。\n\nM-x nova-rime-memory-at-point 可从正文或当前候选查找相关学习词语。\n")))
    (with-help-window "*Nova-rime 学习帮助*"
      (princ text))))

(provide 'nova-rime-memory)
;;; nova-rime-memory.el ends here
