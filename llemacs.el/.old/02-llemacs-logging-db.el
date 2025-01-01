;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 12:16:16
;;; Time-stamp: <2024-12-31 12:16:16 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-logging-db.el

(require '01-llemacs-config)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar llemacs--log-path-db llemacs--path-log-db
  "Alias for logging database path.")

(defvar llemacs--log-db-connection nil
  "Database connection for logging.")

(defvar llemacs--log-db-schema
  '((projects . ((timestamp :not-null)
                 (project-id :not-null)
                 (project-state :not-null)
                 (github-url)
                 (description)
                 (goals)
                 (created-at :not-null)
                 (updated-at :not-null)
                 (workspace-path)
                 (config-path)))
    (milestones . ((timestamp :not-null)
                   (parent-project-id :not-null)
                   (parent-project-goal)
                   (milestone-id :not-null)
                   (milestone-state :not-null)
                   (description)
                   (expected-completion)
                   (completed-at)))
    (tasks . ((timestamp :not-null)
              (parent-project-id :not-null)
              (parent-project-goal)
              (parent-milestone-id :not-null)
              (task-id :not-null)
              (task-state :not-null)
              (priority)
              (retry-count :default 0)))
    (steps . ((timestamp :not-null)
              (parent-project-id :not-null)
              (parent-milestone-id :not-null)
              (parent-task-id :not-null)
              (step-id :not-null)
              (context)
              (generated-code)
              (execution-result)
              (success-message)
              (error-message)
              (git-commit-hash)
              (is-milestone-met)
              (is-goal-met)
              (execution-time))))
  "Database schema for LLEMACS logging system.")

(defun llemacs--log-init-db ()
  "Initialize database for structured logging."
  (unless (file-exists-p (file-name-directory llemacs--log-path-db))
    (make-directory (file-name-directory llemacs--log-path-db) t))
  (setq llemacs--log-db-connection (emacsql-sqlite-open llemacs--log-path-db))
  (dolist (table-spec llemacs--log-db-schema)
    (let ((table-name (car table-spec))
          (columns (cdr table-spec)))
      (emacsql llemacs--log-db-connection
               [:create-table-if-not-exists $i1 $v2]
               table-name
               columns)))
  (message "Log database initialized at %s" llemacs--log-path-db))

(provide '02-llemacs-logging-db)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))