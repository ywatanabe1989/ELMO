;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 17:26:19
;;; Time-stamp: <2024-12-29 17:26:19 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/02-llemacs-logging-core.el

(require '01-llemacs-config)
(require '04-llemacs-utils)
;; ----------------------------------------
;; Logging variables
;; ----------------------------------------
(defvar llemacs--log-level 'info
  "Current logging level: 'debug, 'info, 'warn, or 'error.")

;; (defvar llemacs--log-backup-limit 5
;;   "Maximum number of backup files to keep.")

;; -------------------------------------
;; Log file handlers
;; -------------------------------------
(defun llemacs--log-init ()
  "Initialize the log file if it doesn't exist."
  (unless (file-exists-p llemacs--path-logs)
    (make-directory llemacs--path-logs t))
  (unless (file-exists-p llemacs--path-log-system)
    (with-temp-file llemacs--path-log-system
      (insert (format "=== ELMO log initialized: %s ==="
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun llemacs--log-open ()
  "Open the ELMO log file in a buffer."
  (interactive)
  (if (file-exists-p llemacs--path-log-system)
      (progn
        (find-file-read-only llemacs--path-log-system)
        (goto-char (point-min)))
    (message "Log file does not exist: %s" llemacs--path-log-system)))

(defun llemacs-log-backup ()
  "Backup current log file and create new one."
  (interactive)
  (when (file-exists-p llemacs--path-log-system)
    (unless (file-exists-p llemacs--path-log-backups)
      (make-directory llemacs--path-log-backups t))
    (let ((backup-name (format "system-%s.log"
                               (format-time-string "%Y%m%d-%H%M%S"))))
      (rename-file llemacs--path-log-system
                   (expand-file-name backup-name llemacs--path-log-backups))
      (llemacs--log-init))))

;; -------------------------------------
;; Loggers
;; -------------------------------------
(defun llemacs--log-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s:%s"
              (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--log-to-file (message log-file &optional level)
  "Log MESSAGE to LOG-FILE with optional LEVEL."
  (llemacs--log-init)
  (let* (;; (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (level-str (if level (format "%s" (upcase (symbol-name level))) ""))
         (caller-info (llemacs--log-get-caller-info))
         (formatted-msg (format "[%s %s %s]\n=> %s\n%s\n"
                                level-str
                                llemacs--timestamp
                                (user-login-name)

                                (or caller-info "unknown")
                                message)))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-min))
      (insert "--------------------------------------------------------------------------------\n")
      (insert formatted-msg)
      (write-region (point-min) (point-max) log-file nil 'quiet)
      (llemacs-rotate-logs-if-needed log-file))))

(defun llemacs--log (level message)
  "Log MESSAGE with LEVEL if it meets current log-level threshold."
  (when (or (eq llemacs--log-level 'debug)
            (memq level '(error warn)))
    (llemacs--log-to-file message llemacs--path-log-system level)))

(defun llemacs--log-message (message)
  "Log general MESSAGE."
  (llemacs--log 'info message))

(defun llemacs--log-error (message)
  "Log error MESSAGE and open the log file."
  (llemacs--log 'error
             (if (stringp message)
                 message
               (error-message-string message)))
  (llemacs--log-open))

(defun llemacs--log-warning (message)
  "Log warning MESSAGE."
  (llemacs--log 'warn message))

(defun llemacs--log-debug (message)
  "Log debug MESSAGE."
  (llemacs--log 'debug message))

(defun llemacs--log-prompt (_message)
  "Log prompt MESSAGE."
  (llemacs--log 'info (concat "[PROMPT] " _message)))

(defun llemacs--log-success (_message)
  "Log success MESSAGE."
  (llemacs--log 'info (concat "[SUCCESS] " _message)))

(defun llemacs--log-system (_message)
  "Log system MESSAGE."
  (llemacs--log 'info (concat "[SYSTEM] " _message)))

;; -------------------------------------
;; I don't understand
;; -------------------------------------
(defun llemacs-rotate-logs-if-needed (log-file)
  "Rotate LOG-FILE if it exceeds size limit."
  (when (and (file-exists-p log-file)
             (> (file-attribute-size (file-attributes log-file))
                (* 1024 1024)))
    (let* ((backup-name (format "%s.%s"
                                log-file
                                (format-time-string "%Y%m%d-%H%M%S")))
           (backup-dir (expand-file-name "backups"
                                         (file-name-directory log-file))))
      (unless (file-exists-p backup-dir)
        (make-directory backup-dir t))
      (rename-file log-file
                   (expand-file-name (file-name-nondirectory backup-name)
                                     backup-dir))
      (llemacs--log-remove-old backup-dir))))


(provide '02-llemacs-logging-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))