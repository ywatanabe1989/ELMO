;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 20:14:25
;;; Time-stamp: <2024-12-31 20:14:25 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-config/01-llemacs-config-paths.el

;; ----------------------------------------
;; Path
;; ----------------------------------------
;; Global
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

;; Logging
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

;; Scripts
(defcustom llemacs--path-scripts-python
  (expand-file-name "resources/scripts/python/" llemacs-path-workspace)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs-logging)

;; Projects
(defcustom llemacs-path-projects
  (expand-file-name "projects" llemacs-path-workspace)
  "Base directory for LLEMACS projects."
  :type 'string
  :group 'llemacs-project)

;; Resources
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


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))