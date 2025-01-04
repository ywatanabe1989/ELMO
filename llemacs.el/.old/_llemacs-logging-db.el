;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-30 23:13:41
;;; Time-stamp: <2024-12-30 23:13:41 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/_llemacs-logging-db.el

(require '01-llemacs-config)
(require '04-llemacs-utils)
(require 'emacsql)
(require 'emacsql-sqlite)

(defcustom llemacs--path-log-db
  (expand-file-name "logs/llemacs-logs.db" llemacs-path-workspace)
  "Path to SQLite database file for logs."
  :type 'file
  :group 'llemacs)

(defvar llemacs--log-db nil
  "Database connection for logging.")

(defvar llemacs--log-levels
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3))
  "Mapping of log levels to numeric values for easier querying.")

(defun llemacs--log-db-init ()
  "Initialize logging database."
  (unless (file-exists-p (file-name-directory llemacs--path-log-db))
    (make-directory (file-name-directory llemacs--path-log-db) t))
  (setq llemacs--log-db (emacsql-sqlite-open llemacs--path-log-db))
  (emacsql llemacs--log-db
           [:create-table-if-not-exists logs
                                        ([timestamp
                                          level
                                          project
                                          task
                                          user
                                          message
                                          input
                                          output
                                          caller_info])])
  (message "Log database initialized at %s" llemacs--path-log-db))

(defun llemacs--log-to-db (level project task message &optional input output caller-info)
  "Log to database with LEVEL, PROJECT, TASK, MESSAGE, and optional INPUT, OUTPUT, CALLER-INFO."
  (unless llemacs--log-db (llemacs--log-db-init))
  (emacsql llemacs--log-db
           [:create-table-if-not-exists logs
                                        ([timestamp
                                          level
                                          project
                                          task
                                          user
                                          message
                                          input
                                          output
                                          caller_info])])

  (defun llemacs--log (level message &optional project task input output)
    "Enhanced logging with LEVEL, MESSAGE, optional PROJECT, TASK, INPUT, OUTPUT."
    (llemacs--log-to-db level
                     (or project 'system)
                     (or task 'default)
                     message
                     input
                     output))

(defun llemacs-log-user-action (action &optional project task input output)
  "Log user ACTION with optional PROJECT, TASK, INPUT, OUTPUT."
  (llemacs--log 'info action project task input output))

(defun llemacs-log-system-event (event &optional project task input output)
  "Log system EVENT with optional PROJECT, TASK, INPUT, OUTPUT."
  (llemacs--log 'info event project task input output))

(defun llemacs-logs-get-by-level (level &optional limit)
  "Get logs with LEVEL, optionally limited to LIMIT entries."
  (emacsql llemacs--log-db
           [:select [timestamp level project task user message input output caller_info]
                    :from logs
                    :where (= level $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           level (or limit 50)))

(defun llemacs-logs-get-by-project (project &optional limit)
  "Get logs for PROJECT, optionally limited to LIMIT entries."
  (emacsql llemacs--log-db
           [:select [timestamp level project task user message input output caller_info]
                    :from logs
                    :where (= project $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           project (or limit 50)))

(defun llemacs-logs-get-recent (&optional limit)
  "Get recent logs, optionally limited to LIMIT entries."
  (emacsql llemacs--log-db
           [:select [timestamp level project task user message input output caller_info]
                    :from logs
                    :order-by [(desc timestamp)]
                    :limit $s1]
           (or limit 50)))

;; ;; Initialize the database
;; (llemacs--log-db-init)
;; ;; Log user actions
;; (llemacs-log-user-action "Started new session")
;; (llemacs-log-user-action "Opened file X" 'editor)
;; (llemacs-log-user-action "Ran test Y" 'testing 'unit-test)
;; ;; Log system events
;; (llemacs-log-system-event "System initialized")
;; (llemacs-log-system-event "Cache cleared" 'maintenance)
;; ;; Query logs
;; ;; Get last 10 error logs
;; (llemacs-logs-get-by-level 'error 10)
;; ;; Get last 20 logs from 'editor' project
;; (llemacs-logs-get-by-project 'editor 20)
;; ;; Get last 5 logs regardless of level/project
;; (llemacs-logs-get-recent 5)

(provide '15-llemacs-logging-db)
(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-29 19:36:29
;; ;;; Time-stamp: <2024-12-29 19:36:29 (ywatanabe)>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/15-llemacs-logging-db.el

;; (require '01-llemacs-config)
;; (require '04-llemacs-utils)
;; (require 'emacsql)
;; (require 'emacsql-sqlite)

;; (defcustom llemacs--path-log-db
;;   (expand-file-name "logs/llemacs-logs.db" llemacs-path-workspace)
;;   "Path to SQLite database file for logs."
;;   :type 'file
;;   :group 'llemacs)

;; (defvar llemacs--log-db nil
;;   "Database connection for logging.")

;; (defvar llemacs--log-levels
;;   '((debug . 0)
;;     (info . 1)
;;     (warn . 2)
;;     (error . 3))
;;   "Mapping of log levels to numeric values for easier querying.")


;; (defun llemacs--log-db-init ()
;;   "Initialize logging database."
;;   (unless (file-exists-p (file-name-directory llemacs--path-log-db))
;;     (make-directory (file-name-directory llemacs--path-log-db) t))
;;   (setq llemacs--log-db (emacsql-sqlite-open llemacs--path-log-db))

;;   (emacsql llemacs--log-db
;;            [:create-table-if-not-exists logs
;;                                         ([timestamp
;;                                           level
;;                                           project
;;                                           task
;;                                           user
;;                                           message
;;                                           input
;;                                           output
;;                                           agent_id
;;                                           caller_info])])
;;   (message "Log database initialized at %s" llemacs--path-log-db))


;; (defun llemacs--log-to-db (level project task message &optional input output agent-id caller-info)
;;   "Log to database with LEVEL, PROJECT, TASK, MESSAGE, and optional INPUT, OUTPUT, AGENT-ID, CALLER-INFO."
;;   (unless llemacs--log-db (llemacs--log-db-init))
;;   (emacsql llemacs--log-db
;;            [:insert :into logs
;;                     :values $v1]
;;            (vector llemacs--timestamp
;;                    level
;;                    project
;;                    task
;;                    (user-login-name)
;;                    message
;;                    input
;;                    output
;;                    agent-id
;;                    (or caller-info (llemacs--log-get-caller-info)))))

;; (defun llemacs--log (level message &optional project task input output agent-id)
;;   "Enhanced logging with LEVEL, MESSAGE, optional PROJECT, TASK, INPUT, OUTPUT, and AGENT-ID."
;;   (llemacs--log-to-db level
;;                    (or project 'system)
;;                    (or task 'default)
;;                    message
;;                    input
;;                    output
;;                    agent-id))

;; (defun llemacs-log-user-action (action &optional project task input output agent-id)
;;   "Log user ACTION with optional PROJECT, TASK, INPUT, OUTPUT, and AGENT-ID."
;;   (llemacs--log 'info action project task input output agent-id))

;; (defun llemacs-log-system-event (event &optional project task input output agent-id)
;;   "Log system EVENT with optional PROJECT, TASK, INPUT, OUTPUT, and AGENT-ID."
;;   (llemacs--log 'info event project task input output agent-id))

;; (defun llemacs-log-system-event (event &optional project task)
;;   "Log system EVENT with optional PROJECT and TASK."
;;   (llemacs--log 'info event project task))

;; (defun llemacs-logs-get-by-level (level &optional limit)
;;   "Get logs with LEVEL, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message caller_info]
;;                     :from logs
;;                     :where (= level $s1)
;;                     :order-by [(desc timestamp)]
;;                     :limit $s2]
;;            level (or limit 50)))

;; (defun llemacs-logs-get-by-project (project &optional limit)
;;   "Get logs for PROJECT, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message caller_info]
;;                     :from logs
;;                     :where (= project $s1)
;;                     :order-by [(desc timestamp)]
;;                     :limit $s2]
;;            project (or limit 50)))


;; (defun llemacs-logs-get-recent (&optional limit)
;;   "Get recent logs, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message caller_info]
;;                     :from logs
;;                     :order-by [(desc timestamp)]
;;                     :limit $s1]
;;            (or limit 50)))


;; (defun llemacs-logs-get-by-level (level &optional limit)
;;   "Get logs with LEVEL, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message input output agent_id caller_info]
;;                     :from logs
;;                     :where (= level $s1)
;;                     :order-by [(desc timestamp)]
;;                     :limit $s2]
;;            level (or limit 50)))

;; (defun llemacs-logs-get-by-project (project &optional limit)
;;   "Get logs for PROJECT, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message input output agent_id caller_info]
;;                     :from logs
;;                     :where (= project $s1)
;;                     :order-by [(desc timestamp)]
;;                     :limit $s2]
;;            project (or limit 50)))

;; (defun llemacs-logs-get-recent (&optional limit)
;;   "Get recent logs, optionally limited to LIMIT entries."
;;   (emacsql llemacs--log-db
;;            [:select [timestamp level project task user message input output agent_id caller_info]
;;                     :from logs
;;                     :order-by [(desc timestamp)]
;;                     :limit $s1]
;;            (or limit 50)))

;; ;; ;; Initialize the database
;; ;; (llemacs--log-db-init)

;; ;; ;; Log user actions
;; ;; (llemacs-log-user-action "Started new session")
;; ;; (llemacs-log-user-action "Opened file X" 'editor)
;; ;; (llemacs-log-user-action "Ran test Y" 'testing 'unit-test)

;; ;; ;; Log system events
;; ;; (llemacs-log-system-event "System initialized")
;; ;; (llemacs-log-system-event "Cache cleared" 'maintenance)

;; ;; ;; Query logs
;; ;; ;; Get last 10 error logs
;; ;; (llemacs-logs-get-by-level 'error 10)

;; ;; ;; Get last 20 logs from 'editor' project
;; ;; (llemacs-logs-get-by-project 'editor 20)

;; ;; ;; Get last 5 logs regardless of level/project
;; ;; (llemacs-logs-get-recent 5)

;; (provide '15-llemacs-logging-db)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))