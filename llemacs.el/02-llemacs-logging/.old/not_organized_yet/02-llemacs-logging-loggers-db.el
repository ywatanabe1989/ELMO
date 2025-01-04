;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:51:09
;;; Time-stamp: <2024-12-31 16:51:09 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-loggers-db.el

(require '02-llemacs-logging-db)

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



(provide '02-llemacs-logging-loggers-db)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))