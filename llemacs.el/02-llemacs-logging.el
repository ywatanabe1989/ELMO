;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 00:09:04
;;; Time-stamp: <2024-12-31 00:09:04 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging.el

(require '01-llemacs-config)
(require '01-llemacs-config)
(require 'emacsql)
(require 'emacsql-sqlite)

;; Note that in this system, llm + emacs;
;; Logs are history of processes and source for context generation.
;; Agents are volatile for minimal processing unit (step; context generation -> elisp generation -> elisp execution -> logging
;; Agents will find waiting state of project and tasks
;; Step should be thread safe for the task
;; I am not sure that agent info should be logged
;; Also, I am thinking that git management is crutial


(defvar llemacs--log-level-threshold 'info
  "Minimum log level to record.")

(defun llemacs--log-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info  1)
    ('warn  2)
    ('error 3)
    (_      1)))

(defvar llemacs--db nil
  "Database connection for logging.")

(defvar llemacs--log-structure
  '(timestamp
    level
    project_id
    task_id
    agent_id
    message
    context
    input
    output
    dependencies
    state_changes
    caller_info
    user)
  "Required fields for structured logging.")

(defun llemacs--db-init ()
  "Initialize logging database with structured schema."
  (unless (file-exists-p (file-name-directory llemacs--path-log-db))
    (make-directory (file-name-directory llemacs--path-log-db) t))
  (setq llemacs--db (emacsql-sqlite-open llemacs--path-log-db))
  (emacsql llemacs--db
           [:create-table-if-not-exists logs
                                        ([timestamp
                                          level
                                          project_id
                                          task_id
                                          agent_id
                                          message
                                          context
                                          input
                                          output
                                          dependencies
                                          state_changes
                                          caller_info
                                          user])])
  (message "Log database initialized at %s" llemacs--path-log-db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loggers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--log-to-db (level msg metadata)
  "Write structured log entry to database."
  (unless llemacs--db (llemacs--db-init))
  (emacsql llemacs--db
           [:insert :into logs
                    :values $v1]
           (vector llemacs--timestamp
                   level
                   (alist-get 'project_id metadata)
                   (alist-get 'task_id metadata)
                   (alist-get 'agent_id metadata)
                   msg
                   (alist-get 'context metadata)
                   (alist-get 'input metadata)
                   (alist-get 'output metadata)
                   (alist-get 'dependencies metadata)
                   (alist-get 'state_changes metadata)
                   (llemacs--log-get-caller-info)
                   (user-login-name))))

(defun llemacs--log-to-file (msg file &optional level metadata)
  "Write structured log entry to file."
  (let* ((log-entry `(("timestamp" . ,llemacs--timestamp)
                      ("level" . ,(or level 'info))
                      ("message" . ,msg)
                      ("metadata" . ,metadata)
                      ("caller" . ,(llemacs--log-get-caller-info))
                      ("user" . ,(user-login-name))))
         (log-dir (file-name-directory file)))
    (unless (file-exists-p log-dir)
      (make-directory log-dir t))
    (with-temp-file file
      (when (file-exists-p file)
        (insert-file-contents file))
      (goto-char (point-min))
      (insert (concat (json-encode log-entry) "\n")))))

(defun llemacs--log (level msg &optional metadata)
  "Log with both file and database backends."
  (llemacs--log-to-db level msg metadata)
  (llemacs--log-to-file msg llemacs--path-log-system level metadata))

(defun llemacs-log-action (msg &optional metadata)
  "Log user/system action with context."
  (llemacs--log 'info msg metadata))

(defun llemacs-log-error (msg &optional metadata)
  "Log error with context."
  (llemacs--log 'error msg metadata))

(defun llemacs-log-debug (msg &optional metadata)
  "Log debug message with context."
  (llemacs--log 'debug msg metadata))

(defun llemacs-log-info (msg &optional metadata)
  "Log info message with context."
  (llemacs--log 'info msg metadata))

(defun llemacs-log-warning (msg &optional metadata)
  "Log warning message with context."
  (llemacs--log 'warn msg metadata))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs-logs-get-by-level (level &optional limit)
  "Get logs with specific level."
  (emacsql llemacs--db
           [:select *
                    :from logs
                    :where (= level $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           level (or limit 50)))

(defun llemacs-logs-get-by-project (project-id &optional limit)
  "Get logs for specific project."
  (emacsql llemacs--db
           [:select *
                    :from logs
                    :where (= project_id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           project-id (or limit 50)))

(defun llemacs-logs-get-by-agent (agent-id &optional limit)
  "Get logs for specific agent."
  (emacsql llemacs--db
           [:select *
                    :from logs
                    :where (= agent_id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           agent-id (or limit 50)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs-log-view-by-level (level)
  "View logs filtered by LEVEL in a dedicated buffer."
  (interactive (list (completing-read "Level: " '(debug info warn error))))
  (let ((buf (get-buffer-create "*LLEMACS Logs*"))
        (logs (llemacs-logs-get-by-level level)))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "%s [%s] %s\n"
                        (elt log 0) ; timestamp
                        (elt log 1) ; level
                        (elt log 5)))) ; message
      (display-buffer buf))))

(defun llemacs-log-view-by-project (project-id)
  "View logs filtered by PROJECT-ID in a dedicated buffer."
  (interactive "sProject ID: ")
  (let ((buf (get-buffer-create "*LLEMACS Logs*"))
        (logs (llemacs-logs-get-by-project project-id)))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "%s [%s] %s\n"
                        (elt log 0)
                        (elt log 1)
                        (elt log 5))))
      (display-buffer buf))))

(defun llemacs--log-example ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Examples
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Basic logging
  (llemacs-log-debug "Starting function X")
  (llemacs-log-info "Process completed")
  (llemacs-log-warning "Resource usage high")
  (llemacs-log-error "Connection failed")

  ;; Logging with metadata
  (llemacs-log-info "Task started"
                    '((project_id . "PROJ-001")
                      (task_id . "TASK-123")
                      (agent_id . "AGENT-007")
                      (context . "Data processing pipeline")
                      (input . "raw_data.csv")
                      (dependencies . ["db" "api"])))

  ;; View logs
  (llemacs-log-view-by-level 'error)
  (llemacs-log-view-by-project "PROJ-001")

  ;; Query logs
  (let ((error-logs (llemacs-logs-get-by-level 'error 10)))
    (dolist (log error-logs)
      (message "Error: %s at %s" (elt log 5) (elt log 0))))
  )

(provide '02-llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))