;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 08:45:50
;;; Time-stamp: <2024-12-27 08:45:50 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/02-elmo-logging-core.el

(require '01-elmo-config)
(require '04-elmo-utils)

(defvar elmo-logs-dir
  (expand-file-name "logs" elmo-workspace-dir))

(defvar elmo-log-file
  (expand-file-name "global.log" elmo-logs-dir))

(defvar elmo-log-level 'info
  "Current logging level: 'debug, 'info, 'warn, or 'error.")

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

(defun elmo-log-open ()
  "Open the ELMO log file in a buffer."
  (interactive)
  (if (file-exists-p elmo-log-file)
      (progn
        (elmo-get-log-buffer)
        (with-current-buffer elmo-log-buffer-name
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents elmo-log-file)
            (goto-char (point-max))
            (display-buffer (current-buffer)))))
    (message "Log file does not exist: %s" elmo-log-file)))

(defun elmo-log-to-file (message log-file &optional level)
  "Log MESSAGE to LOG-FILE with optional LEVEL."
  (elmo-init-log)
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (level-str (if level (format "[%s]" (upcase (symbol-name level))) ""))
         (formatted-msg (format "[%s]%s %s - @%s\n%s\n"
                                timestamp
                                level-str
                                (user-login-name)
                                (system-name)
                                message)))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-min))
      (insert "--------------------------------------------------------------------------------\n")
      (insert formatted-msg)
      (write-region (point-min) (point-max) log-file nil 'quiet)
      (elmo-rotate-logs-if-needed log-file))))

(defun elmo-rotate-logs-if-needed (log-file)
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
      (elmo-cleanup-old-logs backup-dir))))

(defun elmo-cleanup-old-logs (backup-dir)
  "Remove old logs from BACKUP-DIR keeping only last elmo-backup-limit files."
  (let* ((backup-files (directory-files backup-dir t "\\.log\\."))
         (sorted-files (sort backup-files #'string>)))
    (while (> (length sorted-files) elmo-backup-limit)
      (delete-file (car (last sorted-files)))
      (setq sorted-files (butlast sorted-files)))))

(defun elmo-log (level message)
  "Log MESSAGE with LEVEL if it meets current log-level threshold."
  (when (or (eq elmo-log-level 'debug)
            (memq level '(error warn)))
    (elmo-log-to-file message elmo-log-file level)))

(defun elmo-log-message (message)
  "Log general MESSAGE."
  (elmo-log 'info message))


(defun elmo-log-error (message)
  "Log error MESSAGE and open the log file."
  (elmo-log 'error message)
  (elmo-log-open))

(defun elmo-log-warning (message)
  "Log warning MESSAGE."
  (elmo-log 'warn message))

(defun elmo-log-debug (message)
  "Log debug MESSAGE."
  (elmo-log 'debug message))

(defun elmo-log-prompt (_message)
  "Log prompt MESSAGE."
  (interactive)
  (elmo-log 'info (concat "[PROMPT] " _message)))

(defun elmo-log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (elmo-log 'info (concat "[SUCCESS] " _message)))

;; (defun elmo-log-prompt (_message)
;;   "Log prompt MESSAGE."
;;   (interactive)
;;   (elmo-log-message (concat "[PROMPT] " _message)))

;; (defun elmo-log-success (_message)
;;   "Log success MESSAGE."
;;   (interactive)
;;   (elmo-log-message (concat "[SUCCESS] " _message)))

;; (defun elmo-log-warning (_message)
;;   "Log warning MESSAGE."
;;   (interactive)
;;   (elmo-log-message (concat "[WARNING] " _message)))

;; (defun elmo-log-error (_message)
;;   "Log error MESSAGE and run diagnostic command."
;;   (interactive)
;;   (elmo-log-message (concat "[ERROR] " _message))
;;   (elmo-log-open))

;; (defun elmo-log-note (_message)
;;   "Note MESSAGE to the ELMO."
;;   (interactive)
;;   (elmo-log-message (concat "[NOTE] " _message)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Command
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun elmo-log-command (_message)
;;   "Log command _MESSAGE to the command log file."
;;   (interactive)
;;   (elmo-log-to-file (concat "[COMMAND] " _message) elmo-log-command-file))

;; (defun elmo-log-command-success (_message)
;;   "Log command _MESSAGE to the command log file."
;;   (interactive)
;;   (elmo-log-to-file (concat "[COMMAND SUCCESS] " _message) elmo-log-command-file))

;; (defun elmo-log-command-error (_message)
;;   "Log command _MESSAGE to the command log file."
;;   (interactive)
;;   (elmo-log-to-file (concat "[COMMAND ERROR] " _message) elmo-log-command-file))

;; (defun elmo-log-command-note (_message)
;;   "Log command _MESSAGE to the command log file."
;;   (interactive)
;;   (elmo-log-to-file (concat "[COMMAND NOTE] " _message) elmo-log-command-file))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Log File Management
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun elmo-init-log ()
;;   "Initialize the log file if it doesn't exist."
;;   (interactive)
;;   (unless (file-exists-p elmo-logs-dir)
;;     (make-directory elmo-logs-dir t))
;;   (unless (file-exists-p elmo-log-file)
;;     (with-temp-file elmo-log-file
;;       (insert (format "=== ELMO Log Log Created: %s ===\n\n"
;;                       (format-time-string "%Y-%m-%d %H:%M:%S"))))))

;; (defun elmo-log-open ()
;;   "Open the ELMO log file in a buffer."
;;   (interactive)
;;   (if (file-exists-p elmo-log-file)
;;       (progn
;;         (sleep-for 0.5)
;;         (find-file-noselect elmo-log-file))
;;     (message "Log file does not exist: %s" elmo-log-file)))

;; (provide '02-elmo-logging-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))