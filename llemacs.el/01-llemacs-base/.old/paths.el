;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 14:04:42
;;; Time-stamp: <2025-01-03 14:07:54 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/paths.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs-path
  (expand-file-name "../../" (file-name-directory (or load-file-name buffer-file-name)))
  "Base directory for LLEMACS project."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-workspace
  (expand-file-name "workspace" llemacs-path)
  "Base directory for LLEMACS workspace."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-home
  (expand-file-name (format "agents/%s" (user-login-name)) llemacs-path-workspace)
  "User-specific LLEMACS home directory."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-emacs-server
  (expand-file-name ".emacs.d/emacs-server/server" llemacs-path-home)
  "Path to LLEMACS's Emacs server socket."
  :type 'string
  :group 'llemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-python-env
  (expand-file-name ".env" llemacs-path-workspace)
  "Path to the Python environment used by llemacs.el."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-python
  (expand-file-name "bin/python" llemacs--path-python-env)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories
(defcustom llemacs--path-logging-system-logs
  (expand-file-name "logs" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-logs-by-level
  (expand-file-name "logs/by_level/" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-backups
  (expand-file-name "backups" llemacs--path-logging-system-logs)
  "Directory for LLEMACS log backups."
  :type 'directory
  :group 'llemacs-logging)

;; Global logging files
(defcustom llemacs--path-logging-system-all
  (expand-file-name "logging.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system log file."
  :type 'file
  :group 'llemacs-logging)

;; Level-specific logging files
(defcustom llemacs--path-logging-system-debug
  (expand-file-name "debug.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system debug log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-info
  (expand-file-name "info.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system info log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-success
  (expand-file-name "success.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system success log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-prompt
  (expand-file-name "prompt.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system prompt log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-elisp
  (expand-file-name "elisp.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system elisp log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-api
  (expand-file-name "api.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system api log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-search
  (expand-file-name "search.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system search log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-warn
  (expand-file-name "warn.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system warn log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-error
  (expand-file-name "error.log" llemacs--path-logging-system-logs-by-level)
  "Path to LLEMACS system error log file."
  :type 'file
  :group 'llemacs-logging)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-project-logs nil
  "Base path for project logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-project-logs-by-level nil
  "Base path for project logs by level."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-project-all nil
  "Base path for all project logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-projects
  (expand-file-name "projects" llemacs-path-workspace)
  "Directory for projects."
  :type 'file
  :group 'llemacs-project)

(defcustom llemacs--path-sample-project-zip
  (expand-file-name "000-sample-project.zip" llemacs--path-projects)
  "Sample project zip file for initializing project structure."
  :type 'file
  :group 'llemacs-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-resources
  (expand-file-name "resources" llemacs-path-workspace)
  "Directory for resources."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-templates
  (expand-file-name "resources/templates" llemacs-path-workspace)
  "Directory for templates."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-agents
  (expand-file-name "resources/agents" llemacs-path-workspace)
  "Directory for agent definitions."
  :type 'string
  :group 'llemacs-api)

(defcustom llemacs-path-tools
  (expand-file-name "resources/tools" llemacs-path-workspace)
  "Directory for tool definitions."
  :type 'string
  :group 'llemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-prompt-templates
  (expand-file-name "resources/prompt-templates" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-prompt-compiled
  (expand-file-name "resources/prompt-templates/compiled" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-prompt-components
  (expand-file-name "resources/prompt-templates/components" llemacs-path-workspace)
  "Directory for prompt components."
  :type 'file
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
