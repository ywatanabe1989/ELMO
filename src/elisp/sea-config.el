;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 09:52:33
;;; Time-stamp: <2024-12-07 09:52:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-config.el


;;; Code:

(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup sea nil
  "Self-evolving agent configuration."
  :group 'applications)

(defvar sea--sudo-password nil
  "Temporary storage for sudo password.")

(defvar sea-max-retries 5
  "Maximum number of retries for failed execution.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base directories for the user; defined in sea-config.el and thus commented-out here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; User installation paths
;; (defvar sea-user-root-dir (file-name-directory (directory-file-name
;;                                               (file-name-directory
;;                                                (or load-file-name buffer-file-name))))
;;   "User's SEA installation root directory.")

;; (defvar sea-user-source-dir (expand-file-name "src" sea-user-root-dir)
;;   "User's SEA source directory.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base directories for the SEA user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom sea-user "sea"
  "SEA system user name."
  :type 'string
  :group 'sea)

(defvar sea-uid
  (string-to-number (shell-command-to-string (format "id -u %s" sea-user)))
  "User ID of SEA system user.")

(defcustom sea-work-dir (format "~/.%s" sea-user)
  "SEA working directory."
  :type 'directory
  :group 'sea)

(defcustom sea-home (format "/home/%s" sea-user)
  "SEA user home directory."
  :type 'directory
  :group 'sea)

(defvar sea-workspace-dir (expand-file-name "workspace" sea-work-dir))
(defvar sea-source-dir (expand-file-name "self-evolving-agent/src" sea-workspace-dir))
(defvar sea-backups-dir (expand-file-name "backups" sea-work-dir))
(defvar sea-logs-dir (expand-file-name "logs" sea-work-dir))
(defvar sea-log-file (expand-file-name "history.log" sea-logs-dir))
(defvar sea-command-logs-dir (expand-file-name "command-logs" sea-work-dir))
(defvar sea-log-command-file
  (expand-file-name
   (format "%s.log"
           (format-time-string "%Y%m%d-%H%M%S"))
   sea-command-logs-dir))
(defvar sea-requests-dir (expand-file-name "requests" sea-work-dir))
(defvar sea-config-dir (expand-file-name "config" sea-work-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom sea-emacs-bin "/usr/bin/emacs"
  "Path to Emacs binary."
  :type 'file
  :group 'sea)

(defcustom sea-emacs-cli "/usr/bin/emacsclient"
  "Path to emacsclient binary."
  :type 'file
  :group 'sea)

(defcustom sea-server-script-path
  (expand-file-name "sea_server.sh" sea-user-source-dir)
  "Path to SEA server control script."
  :type 'string
  :group 'sea)
;; Its value is
;; "/home/ywatanabe/.emacs.d/lisp/self-evolving-agent/src/sea_server.sh"

(defvar sea-server-script-output nil
  "Store output from server script calls.")
;; sea-server-script-output’s value is

(defvar sea-log-file (expand-file-name "sea.log" sea-logs-dir)
  "Store output from server script calls.")
;; "/home/ywatanabe/.sea/logs/sea.log"

(defcustom sea-server-socket-dir (format "/tmp/emacs%d" sea-uid)
  "Directory for SEA Emacs server socket.")
;; Its value is "/tmp/emacs999"

(defcustom sea-server-socket-file (format "/tmp/emacs%d/server" sea-uid)
  "File path for SEA Emacs server socket.")
;; Its value is "/tmp/emacs999/server"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitHub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom sea-github-token-file (expand-file-name "github-token" sea-config-dir)
  "Path to GitHub token file. Requires 600 permissions."
  :type 'file
  :group 'sea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom sea-prompts-dir (expand-file-name "prompts" sea-user-source-dir)
  "File for user's improvement requests."
  :type 'file
  :group 'sea)

(defcustom sea-user-request-file (expand-file-name "user-request.md" sea-requests-dir)
  "File for user's improvement requests."
  :type 'file
  :group 'sea)

(defcustom sea-request-file (expand-file-name "sea-request.md" sea-requests-dir)
  "File for SEA's improvement suggestions."
  :type 'file
  :group 'sea)

(defcustom sea-history-file (expand-file-name "history.log" sea-logs-dir)
  "File to store agent history."
  :type 'file
  :group 'sea)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operation modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom sea-readonly-mode t
  "When non-nil, prevent modifications to core agent files."
  :type 'boolean
  :group 'sea)

(defcustom sea-require-approval t
  "When non-nil, require user approval for critical operations."
  :type 'boolean
  :group 'sea)

(defcustom sea-api-timeout 30
  "Timeout in seconds for API calls."
  :type 'integer
  :group 'sea)

(defvar sea-debug nil
  "Enable debug logging when non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LLM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sea-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar sea-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sea-backup-limit 10
  "Maximum number of backups to keep.")

(defvar sea--installation-log-file (expand-file-name "installation.log" sea-logs-dir)
  "Log file for SEA installation.")

(provide 'sea-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
