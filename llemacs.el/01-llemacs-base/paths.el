;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 01:23:08
;;; Time-stamp: <2025-01-03 01:23:08 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/paths.el

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

;; (defcustom llemacs--path-logging-db
;;   (expand-file-name "logging.db" llemacs--path-logging-system-logs)
;;   "Path to SQLite database for structured logging."
;;   :type 'file
;;   :group 'llemacs-logging)

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
;; Project Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Logging Directories
;; (defcustom llemacs--path-project-logs
;;   (expand-file-name "logs" llemacs--path-projects)
;;   "Directory for project-specific log files."
;;   :type 'directory
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-project-logs-by-level
;;   (expand-file-name "logs/by_level" llemacs--path-projects)
;;   "Directory for project-specific log files by level."
;;   :type 'directory
;;   :group 'llemacs-logging)

;; ;; Project logging files
;; (defcustom llemacs--path-project-all
;;   (expand-file-name "project.log" llemacs--path-project-logs)
;;   "Path to project log file."
;;   :type 'file
;;   :group 'llemacs-logging)

;; ;; Level-specific project logging files
;; (defcustom llemacs--path-project-debug
;;   (expand-file-name "debug.log" llemacs--path-project-logs-by-level)
;;   "Path to project debug log file."
;;   :type 'file
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-project-info
;;   (expand-file-name "info.log" llemacs--path-project-logs-by-level)
;;   "Path to project info log file."
;;   :type 'file
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-project-success
;;   (expand-file-name "success.log" llemacs--path-project-logs-by-level)
;;   "Path to project success log file."
;;   :type 'file
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-project-warn
;;   (expand-file-name "warn.log" llemacs--path-project-logs-by-level)
;;   "Path to project warn log file."
;;   :type 'file
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-project-error
;;   (expand-file-name "error.log" llemacs--path-project-logs-by-level)
;;   "Path to project error log file."
;;   :type 'file
;;   :group 'llemacs-logging)

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
  (llemacs--get-project-path project-id llemacs--path-project-logs "logs"))

(defun llemacs--path-project-logs-by-level-get (&optional project-id)
  "Get project logs by level directory for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-logs-by-level "logs/by_level"))

(defun llemacs--path-project-all-get (&optional project-id)
  "Get project all log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-all "project.log"))

(defun llemacs--path-project-debug-get (&optional project-id)
  "Get project debug log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-debug "by_level/debug.log"))

(defun llemacs--path-project-info-get (&optional project-id)
  "Get project info log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-info "by_level/info.log"))

(defun llemacs--path-project-success-get (&optional project-id)
  "Get project success log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-success "by_level/success.log"))

(defun llemacs--path-project-warn-get (&optional project-id)
  "Get project warn log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-warn "by_level/warn.log"))

(defun llemacs--path-project-error-get (&optional project-id)
  "Get project error log path for PROJECT-ID."
  (llemacs--get-project-path project-id llemacs--path-project-error "by_level/error.log"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-projects
  (expand-file-name "projects" llemacs-path-workspace)
  "Base directory for LLEMACS projects."
  :type 'string
  :group 'llemacs-project)

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
(defcustom llemacs-prompt-template-dir
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))