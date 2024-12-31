;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:22:44
;;; Time-stamp: <2024-12-31 17:22:44 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-config.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)

;; ----------------------------------------
;; Prefixes
;; ----------------------------------------

;; 1. Public interfaces:
;; - llemacs-: Public functions/variables (llemacs-run, llemacs-search)
;; - llemacs-path-: Public path vars (llemacs-path-workspace)

;; 2. Internal:
;; - llemacs--: Private functions/vars (llemacs--timestamp)
;; - llemacs--path-: Private paths (llemacs--path-log-db)
;; - llemacs--logging-: Logging internals (llemacs--logging-db)
;; - llemacs--buffer-: Buffer names (llemacs--buffer-main)

;; 3. Groups:
;; - llemacs: Main group
;; - llemacs-logging: Logging subgroup
;; - llemacs-llm: LLM subgroup
;; - llemacs-project: Project subgroup


;; ----------------------------------------
;; Groups
;; ----------------------------------------
(defgroup llemacs nil
  "Emacs LLM Orchestration"
  :group 'applications)

(defgroup llemacs-logging nil
  "LLEMACS logging configuration."
  :group 'llemacs)

(defgroup llemacs-llm nil
  "LLEMACS LLM configuration."
  :group 'llemacs)

(defgroup llemacs-project nil
  "LLEMACS project configuration."
  :group 'llemacs)

(defgroup llemacs-search nil
  "LLEMACS search configuration."
  :group 'llemacs)

(defgroup llemacs-api nil
  "LLEMACS API configuration."
  :group 'llemacs)

;; ----------------------------------------
;; Reference timestamp
;; ----------------------------------------
(defcustom llemacs--timestamp
  (format-time-string "%Y-%m-%d-%H:%M:%S")
  "Timestamp references by LLEMACS"
  :type 'string
  :group 'llemacs)

(defun llemacs--timestamp-get ()
  "Get current timestamp without updating the reference."
  (format-time-string "%Y-%m-%d-%H:%M:%S"))

(defun llemacs-timestamp-update ()
  "Updates the reference timestamp."
  (setq llemacs--timestamp (llemacs--timestamp-get)))

;; ----------------------------------------
;; Path
;; ----------------------------------------
(defcustom llemacs-path-workspace
  "/home/ywatanabe/.emacs.d/lisp/llemacs/workspace"
  "Base directory for LLEMACS workspace."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-home
  (expand-file-name (format "llemacs/%s" (user-login-name)) llemacs-path-workspace)
  "User-specific LLEMACS home directory."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-emacs-server
  (expand-file-name ".emacs.d/emacs-server/server" llemacs-path-home)
  "Path to LLEMACS's Emacs server socket."
  :type 'string
  :group 'llemacs)

(defcustom llemacs--path-logging-logs
  (expand-file-name "logs" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-system
  (expand-file-name "system.log" llemacs--path-logging-logs)
  "Path to LLEMACS system log file."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-logging-db
  (expand-file-name "logging.db" llemacs--path-logging-logs)
  "Path to SQLite database for structured logging."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs--path-log-backups
  (expand-file-name "backups" llemacs--path-logging-logs)
  "Directory for LLEMACS log backups."
  :type 'directory
  :group 'llemacs-logging)


;; (defcustom llemacs--path-log-db
;;   (expand-file-name "elmo-logs.db" llemacs-path-workspace)
;;   "Path to SQLite database for structured logging."
;;   :type 'file
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-logs
;;   (expand-file-name "logs" llemacs-path-workspace)
;;   "Directory for LLEMACS log files."
;;   :type 'directory
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-log-backups
;;   (expand-file-name "logs/backup" llemacs-path-workspace)
;;   "Directory for LLEMACS log backups."
;;   :type 'directory
;;   :group 'llemacs-logging)

;; (defcustom llemacs--path-log-system
;;   (expand-file-name "logs/system.log" llemacs-path-workspace)
;;   "Path to LLEMACS system log file."
;;   :type 'file
;;   :group 'llemacs-logging)

(defcustom llemacs--path-scripts-python
  (expand-file-name "resources/scripts/python/" llemacs-path-workspace)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs-logging)

(defcustom llemacs-path-projects
  (expand-file-name "projects" llemacs-path-workspace)
  "Base directory for LLEMACS projects."
  :type 'string
  :group 'llemacs-project)

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

;; ----------------------------------------
;; Buffers
;; ----------------------------------------
(defcustom llemacs--buffer-main "*LLEMACS*"
  "Name of main buffer."
  :type 'string
  :group 'llemacs)

(defcustom llemacs--buffer-log "*LLEMACS-LOGGING*"
  "Name of log buffer."
  :type 'string
  :group 'llemacs)

(defcustom llemacs--buffer-project "*LLEMACS-PROJECT*"
  "Buffer for project operations."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--buffer-search "*LLEMACS-SEARCH*"
  "Buffer for search results."
  :type 'string
  :group 'llemacs-search)

(defcustom llemacs--buffer-debug "*LLEMACS-DEBUG*"
  "Buffer for debug output when debug mode enabled."
  :type 'string
  :group 'llemacs)

;; ----------------------------------------
;; Debug mode
;; ----------------------------------------
(defcustom llemacs-debug nil
  "When non-nil, enable debug logging and verbose output."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs-verbose nil
  "When non-nil, display additional operation details."
  :type 'boolean
  :group 'llemacs)


(provide '01-llemacs-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))