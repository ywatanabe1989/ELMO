;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:58:11
;;; Time-stamp: <2024-12-31 16:58:11 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-db.el

(require '01-llemacs-config)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar llemacs--logging-path-db llemacs--path-log-db
  "Alias for logging database path.")

(defvar llemacs--logging-db-connection nil
  "Database connection for logging.")

(defvar llemacs--logging-db-schema-projects
  '[(timestamp text not null)
    (project-id text not null)
    (project-state text not null)
    (github-url text)
    (description text)
    (goals text)
    (created-at text not null)
    (updated-at text not null)
    (workspace-path text)
    (config-path text)]
  "Schema for projects table")

(defvar llemacs--logging-db-schema-milestones
  '[(timestamp text not null)
    (parent-project-id text not null)
    (parent-project-goal text)
    (milestone-id text not null)
    (milestone-state text not null)
    (description text)
    (expected-completion text)
    (completed-at text)]
  "Schema for milestones table")

(defvar llemacs--logging-db-schema-tasks
  '[(timestamp text not null)
    (parent-project-id text not null)
    (parent-project-goal text)
    (parent-milestone-id text not null)
    (task-id text not null)
    (task-state text not null)
    (priority integer)
    (retry-count integer default 0)]
  "Schema for tasks table")

(defvar llemacs--logging-db-schema-steps
  '[(timestamp text not null)
    (parent-project-id text not null)
    (parent-milestone-id text not null)
    (parent-task-id text not null)
    (step-id text not null)
    (context text)
    (generated-code text)
    (execution-result text)
    (success-message text)
    (error-message text)
    (git-commit-hash text)
    (is-milestone-met boolean)
    (is-goal-met boolean)
    (execution-time integer)]
  "Schema for steps table")

(defvar llemacs--logging-db-schema
  `((projects ,llemacs--logging-db-schema-projects)
    (milestones ,llemacs--logging-db-schema-milestones)
    (tasks ,llemacs--logging-db-schema-tasks)
    (steps ,llemacs--logging-db-schema-steps))
  "Database schema for LLEMACS logging system.")

(defun llemacs--logging-init-db ()
  "Initialize database for structured logging."
  (unless (file-exists-p (file-name-directory llemacs--logging-path-db))
    (make-directory (file-name-directory llemacs--logging-path-db) t))
  (setq llemacs--logging-db-connection (emacsql-sqlite-builtin llemacs--logging-path-db))
  (dolist (table-spec llemacs--logging-db-schema)
    (let* ((table-name (car table-spec))
           (columns (cadr table-spec))
           (formatted-columns
            (mapcar (lambda (col)
                      (let ((name (car col))
                            (type (cadr col))
                            (constraints (cddr col)))
                        (cons (intern (replace-regexp-in-string "-" "_" (symbol-name name)))
                              (cons (intern (concat ":" (symbol-name type)))
                                    (mapcar (lambda (c) (intern (concat ":" (symbol-name c))))
                                            constraints)))))
                    columns)))
      (emacsql llemacs--logging-db-connection
               [:create-table-if-not-exists $i1 ($v2)]
               table-name
               (vector formatted-columns))))
  (message "Log database initialized at %s" llemacs--logging-path-db))

;; (llemacs--logging-init-db)

(provide '02-llemacs-logging-db)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))