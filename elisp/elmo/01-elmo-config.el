;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 14:47:14
;;; Time-stamp: <2024-12-29 14:47:14 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/01-llemacs-config.el

(require 'json)
(require 'request)
(require 'emacsql)
(require 'emacsql-sqlite)


(defgroup llemacs nil
  "Emacs LLM Orchestration"
  :group 'applications)

;; ----------------------------------------
;; Reference timestamp
;; ----------------------------------------
(defcustom llemacs--timestamp
  (format-time-string "%Y-%m-%d-%H:%M:%S")
  "Timestamp references by ELMO"
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
  "Base directory for ELMO workspace."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-home
  (expand-file-name (format "llemacss/%s" (user-login-name)) llemacs-path-workspace)
  "User-specific ELMO home directory."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-emacs-server
  (expand-file-name ".emacs.d/emacs-server/server" llemacs-path-home)
  "Path to ELMO's Emacs server socket."
  :type 'string
  :group 'llemacs)

(defcustom llemacs--path-logs
  (expand-file-name "logs" llemacs-path-workspace)
  "Directory for ELMO log files."
  :type 'directory
  :group 'llemacs)

(defcustom llemacs--path-log-backups
  (expand-file-name "logs/backup" llemacs-path-workspace)
  "Directory for ELMO log backups."
  :type 'directory
  :group 'llemacs)

(defcustom llemacs--path-log-system
  (expand-file-name "logs/system.log" llemacs-path-workspace)
  "Path to ELMO system log file."
  :type 'file
  :group 'llemacs)

;; ----------------------------------------
;; Buffer name
;; ----------------------------------------
(defcustom llemacs--buffer-name-main
  "*ELMO*"
  "Name of main ELMO interaction buffer."
  :type 'string
  :group 'llemacs)

(defcustom llemacs--buffer-name-log
  "*llemacs--log*"
  "Name of ELMO log buffer."
  :type 'string
  :group 'llemacs)

;; (custom-group-members 'llemacs nil)
(provide '01-llemacs-config)
(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-29 08:40:23
;; ;;; Time-stamp: <2024-12-29 08:40:23 (ywatanabe)>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/01-llemacs-config.el

;; (require 'json)
;; (require 'request)

;; (defgroup llemacs nil
;;   "Emacs LLM Orchestration"
;;   :group 'applications)

;; (defcustom llemacs--timestamp
;;   (format-time-string "%Y-%m-%d-%H:%M:%S")
;;   "Current timestamp in YYYY-MM-DD-HH:MM:SS format."
;;   :type 'string
;;   :group 'llemacs)

;; (defcustom llemacs-path-workspace
;;   "/home/ywatanabe/.emacs.d/lisp/llemacs/workspace"
;;   "ELMO working directory."
;;   :type 'directory
;;   :group 'llemacs)

;; (defcustom llemacs-path-home
;;   (expand-file-name (format "llemacss/%s" (user-login-name)) llemacs-path-workspace)
;;   "ELMO user home directory."
;;   :type 'directory
;;   :group 'llemacs)

;; (defcustom llemacs-path-emacs-server
;;   (expand-file-name ".emacs.d/emacs-server/server" llemacs-path-home)
;;   "ELMO emacs server."
;;   :type 'directory
;;   :group 'llemacs)

;; (defvar llemacs--path-logs
;;   (expand-file-name "logs" llemacs-path-workspace))

;; (defvar llemacs--path-log-backups
;;   (expand-file-name "logs/backup" llemacs-path-workspace))

;; (defvar llemacs--path-log-system
;;   (expand-file-name "logs/system.log" llemacs-path-workspace))

;; (defcustom llemacs--buffer-name-main "*ELMO*"
;;   "ELMO buffer name."
;;   :type 'directory
;;   :group 'llemacs)

;; (defcustom llemacs--buffer-name-log "*llemacs--log*"
;;   "ELMO logging buffer name."
;;   :type 'directory
;;   :group 'llemacs)

;; ;; (custom-group-members 'llemacs nil)

;; (provide '01-llemacs-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))