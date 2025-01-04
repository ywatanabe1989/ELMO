;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:57:24
;;; Time-stamp: <2024-12-31 16:57:24 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-getters-db.el


(require '02-llemacs-logging-db)

(defun llemacs--logging-get-project-logs (project-id &optional limit)
  "Get logs for PROJECT-ID with optional LIMIT."
  (emacsql llemacs--logging-db-connection
           [:select * :from projects
                    :where (= project-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           project-id (or limit 50)))

(defun llemacs--logging-get-milestone-logs (milestone-id &optional limit)
  "Get logs for MILESTONE-ID with optional LIMIT."
  (emacsql llemacs--logging-db-connection
           [:select * :from milestones
                    :where (= milestone-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           milestone-id (or limit 50)))

(defun llemacs--logging-get-task-logs (task-id &optional limit)
  "Get logs for TASK-ID with optional LIMIT."
  (emacsql llemacs--logging-db-connection
           [:select * :from tasks
                    :where (= task-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           task-id (or limit 50)))

(defun llemacs--logging-get-step-logs (step-id &optional limit)
  "Get logs for STEP-ID with optional LIMIT."
  (emacsql llemacs--logging-db-connection
           [:select * :from steps
                    :where (= step-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           step-id (or limit 50)))

(defun llemacs--logging-get-project-chain (project-id)
  "Get all logs in project chain for PROJECT-ID."
  (let ((project-logs (llemacs--logging-get-project-logs project-id))
        (milestone-logs (emacsql llemacs--logging-db-connection
                                 [:select * :from milestones
                                          :where (= parent-project-id $s1)
                                          :order-by [(desc timestamp)]]
                                 project-id))
        (task-logs (emacsql llemacs--logging-db-connection
                            [:select * :from tasks
                                     :where (= parent-project-id $s1)
                                     :order-by [(desc timestamp)]]
                            project-id))
        (step-logs (emacsql llemacs--logging-db-connection
                            [:select * :from steps
                                     :where (= parent-project-id $s1)
                                     :order-by [(desc timestamp)]]
                            project-id)))
    `((project . ,project-logs)
      (milestones . ,milestone-logs)
      (tasks . ,task-logs)
      (steps . ,step-logs))))

(provide '02-llemacs-logging-getters-db)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))