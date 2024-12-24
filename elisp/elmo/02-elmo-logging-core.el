;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-25 01:34:51
;;; Time-stamp: <2024-12-25 01:34:51 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/02-elmo-logging-core.el

(require '01-elmo-config)

(defvar elmo-logs-dir
  (expand-file-name "logs" elmo-workspace-dir))

(defvar elmo-log-file
  (expand-file-name "general.log" elmo-logs-dir))

(defvar elmo-log-command-file
  (expand-file-name
   (format "%s.log"
           (format-time-string "%Y%m%d-%H%M%S"))
   elmo-logs-dir))

(defvar elmo-backup-limit 10
  "Maximum number of backups to keep.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmo-log-to-file (message log-file)
  "Log MESSAGE to specified LOG-FILE."
  (elmo-init-log)
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-min))
      (insert (format "[%s] (%s) %s\n" timestamp (user-login-name) message))
      (write-region (point-min) (point-max) log-file nil 'quiet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmo-log-message (message)
  "Log MESSAGE to the default log file."
  (elmo-log-to-file message elmo-log-file))

(defun elmo-log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (elmo-log-message (concat "[SUCCESS] " _message)))

(defun elmo-log-warning (_message)
  "Log warning MESSAGE."
  (interactive)
  (elmo-log-message (concat "[WARNING] " _message)))

(defun elmo-log-error (_message)
  "Log error MESSAGE and run diagnostic command."
  (interactive)
  (elmo-log-message (concat "[ERROR] " _message))
  (shell-command (format "head -n 20 %s" elmo-log-file) "*Error-Log*"))

(defun elmo-log-note (_message)
  "Note MESSAGE to the ELMO."
  (interactive)
  (elmo-log-message (concat "[NOTE] " _message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmo-log-command (_message)
  "Log command _MESSAGE to the command log file."
  (interactive)
  (elmo-log-to-file (concat "[COMMAND] " _message) elmo-log-command-file))

(defun elmo-log-command-success (_message)
  "Log command _MESSAGE to the command log file."
  (interactive)
  (elmo-log-to-file (concat "[COMMAND SUCCESS] " _message) elmo-log-command-file))

(defun elmo-log-command-error (_message)
  "Log command _MESSAGE to the command log file."
  (interactive)
  (elmo-log-to-file (concat "[COMMAND ERROR] " _message) elmo-log-command-file))

(defun elmo-log-command-note (_message)
  "Log command _MESSAGE to the command log file."
  (interactive)
  (elmo-log-to-file (concat "[COMMAND NOTE] " _message) elmo-log-command-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log File Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmo-init-log ()
  "Initialize the log file if it doesn't exist."
  (interactive)
  (unless (file-exists-p elmo-logs-dir)
    (make-directory elmo-logs-dir t))
  (unless (file-exists-p elmo-log-file)
    (with-temp-file elmo-log-file
      (insert (format "=== ELMO Log Log Created: %s ===\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun elmo-get-log (&optional n_head)
  "Read last N_head lines from agent log file using head command. Default is 32 lines."
  (interactive)
  (when (and elmo-log-file (file-exists-p elmo-log-file))
    (condition-case err
        (let ((output (shell-command-to-string
                       (format "head -%d %s" (or n_head 32) elmo-log-file))))
          (when output
            (concat "## Recent Logs\n\n" output)))
      (error
       (elmo-log-message (format "Error reading log: %s" err))
       nil))))

(defun elmo-show-log ()
  "Show agent log using head -f."
  (interactive)
  (when (and elmo-log-file (file-exists-p elmo-log-file))
    (async-shell-command
     (format "head %s" (shell-quote-argument elmo-log-file))
     "*ELMO Log*")))

(provide '02-elmo-logging-core)

                                        ;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))