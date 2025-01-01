;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:49:27
;;; Time-stamp: <2024-12-31 16:49:27 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-core.el

(require '02-llemacs-logging-db)

(defvar llemacs--logging-level-threshold 'info
  "Minimum log level to record.")

(defun llemacs--logging-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info 1)
    ('search 1)
    ('api 1)
    ('prompt 1)
    ('warn 2)
    ('success 3)
    ('error 4)
    (_ 1)))

(defcustom llemacs-logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs-logging-backup-count 5
  "Number of log backup files to keep."
  :type 'integer
  :group 'llemacs-logging)


;; ----------------------------------------
;; DB-based loggers: For structured project data with relationships (project->milestone->task->step hierarchy)
;; ----------------------------------------
(defun llemacs--logging-log-to-db (data table schema)
  "Log entry with DATA to TABLE using SCHEMA."
  (unless llemacs--logging-db-connection (llemacs--logging-init-db))
  (let ((values (apply #'vector
                       (mapcar (lambda (field) (alist-get field data))
                               (mapcar (lambda (col)
                                         (intern (replace-regexp-in-string "-" "_"
                                                                           (symbol-name (car col)))))
                                       schema)))))
    (emacsql llemacs--logging-db-connection
             [:insert :into $i1 :values $v2]
             table (list values))))

(defun llemacs--logging-log-project (data)
  "Log project entry with DATA based on `llemacs--logging-db-schema-projects`"
  (llemacs--logging-log-to-db data 'projects llemacs--logging-db-schema-projects))

(defun llemacs--logging-log-milestone (data)
  "Log milestone entry with DATA based on `llemacs--logging-db-schema-milestones`"
  (llemacs--logging-log-to-db data 'milestones llemacs--logging-db-schema-milestones))

(defun llemacs--logging-log-task (data)
  "Log task entry with DATA based on `llemacs--logging-db-schema-tasks`"
  (llemacs--logging-log-to-db data 'tasks llemacs--logging-db-schema-tasks))

(defun llemacs--logging-log-step (data)
  "Log step entry with DATA based on `llemacs--logging-db-schema-steps`"
  (llemacs--logging-log-to-db data 'steps llemacs--logging-db-schema-steps))

;; ;; Example usage
;; (llemacs--logging-log-project
;;  '((id . nil)
;;    (timestamp . "2024-01-01T12:00:00")
;;    (project_id . "proj-001")
;;    (project_state . "active")
;;    (github_url . "https://github.com/user/repo")
;;    (description . "Project description")
;;    (goals . "Project goals")
;;    (created_at . "2024-01-01T12:00:00")
;;    (updated_at . "2024-01-01T12:00:00")
;;    (workspace_path . "/path/to/workspace")
;;    (config_path . "/path/to/config")))

;; (llemacs--logging-log-milestone
;;  '((timestamp . "2024-01-01T12:00:00")
;;    (parent_project_id . "proj-001")
;;    (parent_project_goal . "Complete feature X")
;;    (milestone_id . "MS-001")
;;    (milestone_state . "in-progress")
;;    (description . "Implementation milestone")
;;    (expected_completion . "2024-02-01")
;;    (completed_at . nil)))

;; (llemacs--logging-log-task
;;  '((timestamp . "2024-01-01T12:00:00")
;;    (parent_project_id . "proj-001")
;;    (parent_project_goal . "Complete feature X")
;;    (parent_milestone_id . "MS-001")
;;    (task_id . "TASK-001")
;;    (task_state . "pending")
;;    (priority . 1)
;;    (retry_count . 0)))

;; (llemacs--logging-log-step
;;  '((timestamp . "2024-01-01T12:00:00")
;;    (parent_project_id . "proj-001")
;;    (parent_milestone_id . "MS-001")
;;    (parent_task_id . "TASK-001")
;;    (step_id . "STEP-001")
;;    (context . "Initial implementation")
;;    (generated_code . "code here...")
;;    (execution_result . "success")
;;    (success_message . "Step completed")
;;    (error_message . nil)
;;    (git_commit_hash . "abc123")
;;    (is_milestone_met . nil)
;;    (is_goal_met . nil)
;;    (execution_time . 120)))

;; ----------------------------------------
;; File-based loggers: For general system events, debugging, errors (like traditional log files)
;; ----------------------------------------
(defun llemacs--logging-rotate-if-needed (file)
  "Rotate FILE if it exceeds maximum size by appending timestamp."
  (when (and (file-exists-p file)
             (> (file-attribute-size (file-attributes file))
                llemacs-logging-max-size))
    (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (base (file-name-sans-extension file))
           (ext (file-name-extension file)))
      (rename-file file (format "%s-%s.%s" base timestamp ext) t))))

(defun llemacs--logging-log-to-file (message file)
  "Write MESSAGE to specified log FILE."
  (with-temp-buffer
    (insert message "\n")
    (llemacs--logging-rotate-if-needed file)
    (append-to-file (point-min) (point-max) file)))

(defun llemacs--logging-format-message (level message)
  "Format log message with LEVEL and MESSAGE."
  (format "[%s][%s][%s] %s"
          (format-time-string "%Y-%m-%d %H:%M:%S")
          (upcase (symbol-name level))
          (llemacs--log-get-caller-info)
          message))

(defun llemacs--log-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s:%s"
              (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--logging-write (level message &optional file)
  "Write log with LEVEL and MESSAGE to optional FILE."
  (when (<= (llemacs--logging-level-value llemacs--logging-level-threshold)
            (llemacs--logging-level-value level))
    (let ((log-file (or file llemacs--path-log-system)))
      (llemacs--logging-rotate-if-needed log-file)
      (llemacs--logging-log-to-file
       (llemacs--logging-format-message level message)
       log-file)))
  (when (memq level '(error warn search api prompt))
    (llemacs--logging-open-log-file)))

(defun llemacs--logging-debug (message)
  "Log debug MESSAGE."
  (llemacs--logging-write 'debug message))

(defun llemacs--logging-info (message)
  "Log info MESSAGE."
  (llemacs--logging-write 'info message))

(defun llemacs--logging-search (query)
  "Log search QUERY."
  (llemacs--logging-write 'search query))

(defun llemacs--logging-api-call (provider method)
  "Log API call to PROVIDER using METHOD."
  (llemacs--logging-write 'api (format "%s - %s" provider method)))

(defun llemacs--logging-prompt (prompt)
  "Log user PROMPT."
  (llemacs--logging-write 'prompt prompt))

(defun llemacs--logging-warn (message)
  "Log warning MESSAGE."
  (llemacs--logging-write 'warn message))

(defun llemacs--logging-error (message)
  "Log error MESSAGE."
  (llemacs--logging-write 'error message))

(defun llemacs--logging-success (message)
  "Log success MESSAGE."
  (llemacs--logging-write 'success message))

(defun llemacs--logging-open-log-file ()
  "Open the log file in a split window."
  (let* ((log-buffer (find-file-noselect llemacs--path-log-system))
         (window (split-window-below -10)))
    (set-window-buffer window log-buffer)
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (recenter -1))
    (select-window window)))

;; (llemacs--logging-info "aaa")
;; (llemacs--logging-success "aaa")
;; (llemacs--logging-error "aaa")
;; (llemacs--logging-open-log-file)




;; ;; ;; (defun llemacs--logging-query-history (query)
;; ;; ;;   "Log QUERY to history database."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (emacsql llemacs--logging-db-connection
;; ;; ;;            [:insert :into queries :values $v1]
;; ;; ;;            (vector (current-time-string) query)))


;; ;; ;; (defun llemacs--logging-analyze-queries (since)
;; ;; ;;   "Analyze query patterns from SINCE timestamp."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (emacsql llemacs--logging-db-connection
;; ;; ;;            [:select query count_star from
;; ;; ;;                     (select query count (*) as count_star from queries
;; ;; ;;                             where timestamp > $s1
;; ;; ;;                             group by query
;; ;; ;;                             order by count_star desc
;; ;; ;;                             limit 10)]
;; ;; ;;            since))

;; ;; ;; (defun llemacs--logging-get-recent-activity (limit)
;; ;; ;;   "Get recent activity with LIMIT entries."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (emacsql llemacs--logging-db-connection
;; ;; ;;            [:select * from
;; ;; ;;                     (select timestamp type content from activities
;; ;; ;;                             order by timestamp desc
;; ;; ;;                             limit $s1)]
;; ;; ;;            limit))

;; ;; ;; (defun llemacs--logging-performance-metrics ()
;; ;; ;;   "Calculate system performance metrics."
;; ;; ;;   (list :memory (memory-info)
;; ;; ;;         :gc-stats (garbage-collect)
;; ;; ;;         :uptime (emacs-uptime)))

;; ;; ;; (defun llemacs--logging-system-status ()
;; ;; ;;   "Log current system status."
;; ;; ;;   (llemacs-logging-info
;; ;; ;;    (format "Status: %S" (llemacs--logging-performance-metrics))))

;; ;; ;; (defun llemacs--logging-clear-old-entries (days)
;; ;; ;;   "Clear log entries older than DAYS."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (let ((cutoff (time-subtract (current-time)
;; ;; ;;                                (days-to-time days))))
;; ;; ;;     (emacsql llemacs--logging-db-connection
;; ;; ;;              [:delete from activities
;; ;; ;;                       where timestamp < $s1]
;; ;; ;;              (format-time-string "%Y-%m-%d %H:%M:%S" cutoff))))

;; ;; ;; (defun llemacs--logging-export-to-csv (table file)
;; ;; ;;   "Export TABLE to CSV FILE."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (let ((data (emacsql llemacs--logging-db-connection
;; ;; ;;                        [:select * from $i1]
;; ;; ;;                        table)))
;; ;; ;;     (with-temp-file file
;; ;; ;;       (insert (mapconcat #'prin1-to-string (car data) ",") "\n")
;; ;; ;;       (dolist (row (cdr data))
;; ;; ;;         (insert (mapconcat #'prin1-to-string row ",") "\n")))))

;; ;; (provide '02-llemacs-logging-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))