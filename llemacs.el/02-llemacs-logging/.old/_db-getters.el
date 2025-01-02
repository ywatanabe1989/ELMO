;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:54:59
;;; Time-stamp: <2025-01-02 10:54:59 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/db-getters.el

;; (require '02-llemacs-logging-core-db)

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

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))