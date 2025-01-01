;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 12:07:27
;;; Time-stamp: <2024-12-31 12:07:27 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging.el

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
;; I think logging systems should levels: system-level, project-level, ...
;; Do you think only one table is better?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar llemacs--log-path-db llemacs--path-log-db
  "Alias for logging database path.")

(defvar llemacs--log-log-level-threshold 'info
  "Minimum log level to record.")

(defun llemacs--log-log-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info  1)
    ('warn  2)
    ('error 3)
    (_      1)))

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

(defvar llemacs--log-project-log-structure
  (mapcar #'car (alist-get 'projects llemacs--log-db-schema))
  "Structure for project-level logging.
See `llemacs--log-db-schema' for field descriptions.")

(defvar llemacs--log-milestone-log-structure
  (mapcar #'car (alist-get 'milestones llemacs--log-db-schema))
  "Structure for milestone-level logging.
See `llemacs--log-db-schema' for field descriptions.")

(defvar llemacs--log-task-log-structure
  (mapcar #'car (alist-get 'tasks llemacs--log-db-schema))
  "Structure for task-level logging.
See `llemacs--log-db-schema' for field descriptions.")

(defvar llemacs--log-step-log-structure
  (mapcar #'car (alist-get 'steps llemacs--log-db-schema))
  "Structure for step-level logging.
See `llemacs--log-db-schema' for field descriptions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--log-log-project (data)
  "Log project entry with DATA."
  (unless llemacs--log-db-connection (llemacs--log-init-db))
  (let ((values (mapcar (lambda (field) (alist-get field data))
                        llemacs--project-log-structure)))
    (emacsql llemacs--log-db-connection
             [:insert :into projects :values $v1]
             values)))

(defun llemacs--log-log-milestone (data)
  "Log milestone entry with DATA."
  (unless llemacs--log-db-connection (llemacs--log-init-db))
  (let ((values (mapcar (lambda (field) (alist-get field data))
                        llemacs--milestone-log-structure)))
    (emacsql llemacs--log-db-connection
             [:insert :into milestones :values $v1]
             values)))

(defun llemacs--log-log-task (data)
  "Log task entry with DATA."
  (unless llemacs--log-db-connection (llemacs--log-init-db))
  (let ((values (mapcar (lambda (field) (alist-get field data))
                        llemacs--task-log-structure)))
    (emacsql llemacs--log-db-connection
             [:insert :into tasks :values $v1]
             values)))

(defun llemacs--log-log-step (data)
  "Log step entry with DATA."
  (unless llemacs--log-db-connection (llemacs--log-init-db))
  (let ((values (mapcar (lambda (field) (alist-get field data))
                        llemacs--step-log-structure)))
    (emacsql llemacs--log-db-connection
             [:insert :into steps :values $v1]
             values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--log-get-project-logs (project-id &optional limit)
  "Get logs for PROJECT-ID with optional LIMIT."
  (emacsql llemacs--log-db-connection
           [:select * :from projects
                    :where (= project-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           project-id (or limit 50)))

(defun llemacs--log-get-milestone-logs (milestone-id &optional limit)
  "Get logs for MILESTONE-ID with optional LIMIT."
  (emacsql llemacs--log-db-connection
           [:select * :from milestones
                    :where (= milestone-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           milestone-id (or limit 50)))

(defun llemacs--log-get-task-logs (task-id &optional limit)
  "Get logs for TASK-ID with optional LIMIT."
  (emacsql llemacs--log-db-connection
           [:select * :from tasks
                    :where (= task-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           task-id (or limit 50)))

(defun llemacs--log-get-step-logs (step-id &optional limit)
  "Get logs for STEP-ID with optional LIMIT."
  (emacsql llemacs--log-db-connection
           [:select * :from steps
                    :where (= step-id $s1)
                    :order-by [(desc timestamp)]
                    :limit $s2]
           step-id (or limit 50)))

(defun llemacs--log-search (query)
  "Log search QUERY."
  (llemacs--log-to-file
   (format "[SEARCH] %s" query)
   llemacs--path-log-system))

(defun llemacs--log-api-call (provider method)
  "Log API call to PROVIDER using METHOD."
  (llemacs--log-to-file
   (format "[API] %s - %s" provider method)
   llemacs--path-log-system))

(defun llemacs--log-prompt (prompt)
  "Log user PROMPT."
  (llemacs--log-to-file
   (format "[PROMPT] %s" prompt)
   llemacs--path-log-system))

(defun llemacs--log-success (message)
  "Log success MESSAGE."
  (llemacs--log-to-file
   (format "[SUCCESS] %s" message)
   llemacs--path-log-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--log-view-project-logs (project-id)
  "View logs for PROJECT-ID in buffer."
  (interactive "sProject ID: ")
  (let* ((logs (llemacs--log-get-project-logs project-id))
         (buf (get-buffer-create "*LLEMACS Project Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "[%s] Project %s: %s\n"
                        (elt log 0)  ; timestamp
                        (elt log 1)  ; project-id
                        (elt log 4)))) ; description
      (display-buffer buf))))

(defun llemacs--log-view-milestone-logs (milestone-id)
  "View logs for MILESTONE-ID in buffer."
  (interactive "sMilestone ID: ")
  (let* ((logs (llemacs--log-get-milestone-logs milestone-id))
         (buf (get-buffer-create "*LLEMACS Milestone Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "[%s] Milestone %s: %s\n"
                        (elt log 0)  ; timestamp
                        (elt log 3)  ; milestone-id
                        (elt log 5)))) ; description
      (display-buffer buf))))


(defun llemacs--log-view-task-logs (task-id)
  "View logs for TASK-ID in buffer."
  (interactive "sTask ID: ")
  (let* ((logs (llemacs--log-get-task-logs task-id))
         (buf (get-buffer-create "*LLEMACS Task Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (let ((timestamp (elt log 0))
              (task-id (elt log 4))
              (state (elt log 5)))
          (insert (format "[%s] Task %s: %s\n"
                          timestamp task-id state))))
      (display-buffer buf))))

(defun llemacs--log-view-step-logs (step-id)
  "View logs for STEP-ID in buffer."
  (interactive "sStep ID: ")
  (let* ((logs (llemacs--log-get-step-logs step-id))
         (buf (get-buffer-create "*LLEMACS Step Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (let ((timestamp (elt log 0))
              (step-id (elt log 4))
              (result (elt log 7)))
          (insert (format "[%s] Step %s: %s\n"
                          timestamp step-id result))))
      (display-buffer buf))))

(defun llemacs--log-get-project-chain (project-id)
  "Get all logs in project chain for PROJECT-ID."
  (let ((project-logs (llemacs--log-get-project-logs project-id))
        (milestone-logs (emacsql llemacs--log-db-connection
                                 [:select * :from milestones
                                          :where (= parent-project-id $s1)
                                          :order-by [(desc timestamp)]]
                                 project-id))
        (task-logs (emacsql llemacs--log-db-connection
                            [:select * :from tasks
                                     :where (= parent-project-id $s1)
                                     :order-by [(desc timestamp)]]
                            project-id))
        (step-logs (emacsql llemacs--log-db-connection
                            [:select * :from steps
                                     :where (= parent-project-id $s1)
                                     :order-by [(desc timestamp)]]
                            project-id)))
    `((project . ,project-logs)
      (milestones . ,milestone-logs)
      (tasks . ,task-logs)
      (steps . ,step-logs))))

(defun llemacs--log-view-project-chain (project-id)
  "View full project chain for PROJECT-ID."
  (interactive "sProject ID: ")
  (let* ((chain (llemacs--log-get-project-chain project-id))
         (buf (get-buffer-create "*LLEMACS Project Chain*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Project ===\n")
      (dolist (log (alist-get 'project chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 1)
                        (elt log 4))))

      (insert "\n=== Milestones ===\n")
      (dolist (log (alist-get 'milestones chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 3)
                        (elt log 5))))

      (insert "\n=== Tasks ===\n")
      (dolist (log (alist-get 'tasks chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 4)
                        (elt log 5))))

      (insert "\n=== Steps ===\n")
      (dolist (log (alist-get 'steps chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 4)
                        (elt log 7))))

      (display-buffer buf))))

(provide '02-llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))