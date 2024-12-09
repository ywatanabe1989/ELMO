;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 09:52:33
;;; Time-stamp: <2024-12-07 09:52:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-config.el


;;; Code:

(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup semacs nil
  "Self-evolving agent configuration."
  :group 'applications)

(defvar semacs--sudo-password nil
  "Temporary storage for sudo password.")

(defvar semacs-max-retries 5
  "Maximum number of retries for failed execution.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base directories for the user; defined in semacs-config.el and thus commented-out here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; User installation paths
;; (defvar semacs-user-root-dir (file-name-directory (directory-file-name
;;                                               (file-name-directory
;;                                                (or load-file-name buffer-file-name))))
;;   "User's SEMACS installation root directory.")

;; (defvar semacs-user-source-dir (expand-file-name "src" semacs-user-root-dir)
;;   "User's SEMACS source directory.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base directories for the SEMACS user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom semacs-user "semacs"
  "SEMACS system user name."
  :type 'string
  :group 'semacs)

(defvar semacs-uid
  (string-to-number (shell-command-to-string (format "id -u %s" semacs-user)))
  "User ID of SEMACS system user.")

(defcustom semacs-work-dir (format "~/.%s" semacs-user)
  "SEMACS working directory."
  :type 'directory
  :group 'semacs)

(defcustom semacs-home (format "/home/%s" semacs-user)
  "SEMACS user home directory."
  :type 'directory
  :group 'semacs)

(defvar semacs-workspace-dir (expand-file-name "workspace" semacs-work-dir))
(defvar semacs-source-dir (expand-file-name "self-evolving-agent/src" semacs-workspace-dir))
(defvar semacs-backups-dir (expand-file-name "backups" semacs-work-dir))
(defvar semacs-logs-dir (expand-file-name "logs" semacs-work-dir))
(defvar semacs-log-file (expand-file-name "history.log" semacs-logs-dir))
(defvar semacs-command-logs-dir (expand-file-name "command-logs" semacs-work-dir))
(defvar semacs-log-command-file
  (expand-file-name
   (format "%s.log"
           (format-time-string "%Y%m%d-%H%M%S"))
   semacs-command-logs-dir))
(defvar semacs-requests-dir (expand-file-name "requests" semacs-work-dir))
(defvar semacs-config-dir (expand-file-name "config" semacs-work-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom semacs-emacs-bin "/usr/bin/emacs"
  "Path to Emacs binary."
  :type 'file
  :group 'semacs)

(defcustom semacs-emacs-cli "/usr/bin/emacsclient"
  "Path to emacsclient binary."
  :type 'file
  :group 'semacs)

(defcustom semacs-server-script-path
  (expand-file-name "semacs_server.sh" semacs-user-source-dir)
  "Path to SEMACS server control script."
  :type 'string
  :group 'semacs)
;; Its value is
;; "/home/ywatanabe/.emacs.d/lisp/self-evolving-agent/src/semacs_server.sh"

(defvar semacs-server-script-output nil
  "Store output from server script calls.")
;; semacs-server-script-output’s value is

(defvar semacs-log-file (expand-file-name "semacs.log" semacs-logs-dir)
  "Store output from server script calls.")
;; "/home/ywatanabe/.semacs/logs/semacs.log"

(defcustom semacs-server-socket-dir (format "/tmp/emacs%d" semacs-uid)
  "Directory for SEMACS Emacs server socket.")
;; Its value is "/tmp/emacs999"

(defcustom semacs-server-socket-file (format "/tmp/emacs%d/server" semacs-uid)
  "File path for SEMACS Emacs server socket.")
;; Its value is "/tmp/emacs999/server"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitHub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom semacs-github-token-file (expand-file-name "github-token" semacs-config-dir)
  "Path to GitHub token file. Requires 600 permissions."
  :type 'file
  :group 'semacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom semacs-prompts-dir (expand-file-name "prompts" semacs-user-source-dir)
  "File for user's improvement requests."
  :type 'file
  :group 'semacs)

(defcustom semacs-user-request-file (expand-file-name "user-request.md" semacs-requests-dir)
  "File for user's improvement requests."
  :type 'file
  :group 'semacs)

(defcustom semacs-request-file (expand-file-name "semacs-request.md" semacs-requests-dir)
  "File for SEMACS's improvement suggestions."
  :type 'file
  :group 'semacs)

(defcustom semacs-history-file (expand-file-name "history.log" semacs-logs-dir)
  "File to store agent history."
  :type 'file
  :group 'semacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operation modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom semacs-readonly-mode t
  "When non-nil, prevent modifications to core agent files."
  :type 'boolean
  :group 'semacs)

(defcustom semacs-require-approval t
  "When non-nil, require user approval for critical operations."
  :type 'boolean
  :group 'semacs)

(defcustom semacs-api-timeout 30
  "Timeout in seconds for API calls."
  :type 'integer
  :group 'semacs)

(defvar semacs-debug nil
  "Enable debug logging when non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar semacs-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar semacs-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar semacs-backup-limit 10
  "Maximum number of backups to keep.")

(defvar semacs--installation-log-file (expand-file-name "installation.log" semacs-logs-dir)
  "Log file for SEMACS installation.")

(provide 'semacs-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
