;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 03:40:47
;;; Time-stamp: <2025-01-02 03:40:47 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base/paths.el

;; Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs-path
  (expand-file-name "../../" (file-name-directory (or load-file-name buffer-file-name)))
  "Base directory for LLEMACS project."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-workspace
  "/home/ywatanabe/.emacs.d/lisp/llemacs/workspace"
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
(defcustom llemacs--path-python
  (expand-file-name ".env/bin/python" llemacs-path-workspace)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-scripts-python
  (expand-file-name "resources/scripts/python/" llemacs-path-workspace)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs-logging)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-logging-system-logs
  (expand-file-name "logs" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-all
  (expand-file-name "all.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-debug
  (expand-file-name "debug.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system debug log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-info
  (expand-file-name "info.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system info log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-prompt
  (expand-file-name "prompt.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system prompt log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-elisp
  (expand-file-name "elisp.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system elisp log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-api
  (expand-file-name "api.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system api log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-search
  (expand-file-name "search.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system search log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-warn
  (expand-file-name "warn.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system warn log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system-error
  (expand-file-name "error.log" llemacs--path-logging-system-logs)
  "Path to LLEMACS system error log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-db
  (expand-file-name "logging.db" llemacs--path-logging-system-logs)
  "Path to SQLite database for structured logging."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-backups
  (expand-file-name "backups" llemacs--path-logging-system-logs)
  "Directory for LLEMACS log backups."
  :type 'directory
  :group 'llemacs-logging)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs-path-projects
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

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))