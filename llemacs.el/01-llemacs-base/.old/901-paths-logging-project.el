;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 18:54:06
;;; Time-stamp: <2025-01-03 18:54:06 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/path_logging_project.el

;; Project Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-logging-project-debug nil
  "Base path for project debug logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-logging-project-info nil
  "Base path for project info logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-logging-project-prompt nil
  "Base path for project prompt logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-logging-project-success nil
  "Base path for project success logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-logging-project-warn nil
  "Base path for project warn logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-logging-project-error nil
  "Base path for project error logs."
  :type 'string
  :group 'llemacs-project)

;; Function to get project-specific path
(defun llemacs--get-project-path (project-id base-path filename)
  "Get project-specific path for PROJECT-ID using BASE-PATH and FILENAME."
  (if project-id
      (expand-file-name
       filename
       (expand-file-name "logs" (llemacs--proj-get-dir project-id)))
    base-path))

;; Update getter/setter for the variables
(defun llemacs--path-project-logs-get (&optional project-id)
  "Get project logs directory for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-projects-logs "logs"))
;; (llemacs--path-project-logs-get "036-my-project")

(defun llemacs--path-project-logs-by-level-get (&optional project-id)
  "Get project logs by level directory for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-logs-by-level "logs/by_level"))

(defun llemacs--path-project-all-get (&optional project-id)
  "Get project all log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-all "logging.log"))

(defun llemacs--path-project-debug-get (&optional project-id)
  "Get project debug log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-debug "by_level/debug.log"))

(defun llemacs--path-project-info-get (&optional project-id)
  "Get project info log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-info "by_level/info.log"))

(defun llemacs--path-project-prompt-get (&optional project-id)
  "Get project prompt log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-prompt "by_level/prompt.log"))

(defun llemacs--path-project-success-get (&optional project-id)
  "Get project success log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-success "by_level/success.log"))

(defun llemacs--path-project-warn-get (&optional project-id)
  "Get project warn log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-warn "by_level/warn.log"))

(defun llemacs--path-project-error-get (&optional project-id)
  "Get project error log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-error "by_level/error.log"))

(defun llemacs--get-project-path (project-id-or-full-name base-path filename)
  "Get project-specific path for PROJECT-ID using BASE-PATH and FILENAME."
  (if project-id-or-full-name
      (let ((project-id (if (string-match "^\\([0-9]+\\)-" project-id-or-full-name)
                            (match-string 1 project-id-or-full-name)
                          project-id-or-full-name)))
        (expand-file-name
         filename
         (expand-file-name "logs" (llemacs--proj-get-dir project-id))))
    base-path))

(defun llemacs--path-project-logs-get (&optional project-id-or-full-name)
  "Get project logs directory for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-logs "logs"))

(defun llemacs--path-project-logs-by-level-get (&optional project-id-or-full-name)
  "Get project logs by level directory for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-logs-by-level "logs/by_level"))

(defun llemacs--path-project-all-get (&optional project-id-or-full-name)
  "Get project all log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-all "logging.log"))

(defun llemacs--path-project-debug-get (&optional project-id-or-full-name)
  "Get project debug log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-debug "by_level/debug.log"))

(defun llemacs--path-project-info-get (&optional project-id-or-full-name)
  "Get project info log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-info "by_level/info.log"))

(defun llemacs--path-project-prompt-get (&optional project-id-or-full-name)
  "Get project prompt log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-prompt "by_level/prompt.log"))

(defun llemacs--path-project-success-get (&optional project-id-or-full-name)
  "Get project success log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-success "by_level/success.log"))

(defun llemacs--path-project-warn-get (&optional project-id-or-full-name)
  "Get project warn log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-warn "by_level/warn.log"))

(defun llemacs--path-project-error-get (&optional project-id-or-full-name)
  "Get project error log path for PROJECT-ID."
  (llemacs--get-project-path project-id-or-full-name llemacs--path-project-error "by_level/error.log"))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))