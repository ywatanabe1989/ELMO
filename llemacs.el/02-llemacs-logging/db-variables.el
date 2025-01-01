;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 02:57:58
;;; Time-stamp: <2025-01-02 02:57:58 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/db-variables.el

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

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))