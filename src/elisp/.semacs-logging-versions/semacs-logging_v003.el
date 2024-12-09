;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 03:38:52
;;; Time-stamp: <2024-12-06 03:38:52 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-logging.el


(require 'semacs-config)
(require 'semacs-server)


(defun semacs--log-to-file (message log-file)
  "Log MESSAGE to specified LOG-FILE."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-min))
      (insert (format "[%s] %s\n" timestamp message))
      (write-region (point-min) (point-max) log-file nil 'quiet))))

(defun semacs--log-message (message)
  "Log MESSAGE to the default log file."
  (semacs--log-to-file message semacs-log-file))

(defun semacs--log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (semacs--log-message (concat "[SUCCESS] " (message "%s" _message))))

(defun semacs--log-warning (_message)
  "Log warning MESSAGE."
  (interactive)
  (semacs--log-message (concat "[WARNING] " (message "%s" _message))))

(defun semacs--log-error (_message)
  "Log error MESSAGE and run diagnostic command."
  (interactive)
  (semacs--log-message (concat "[ERROR] " (message "%s" _message)))
  (shell-command "head -n 20 ~/.semacs/logs/history.log" "*Error-Log*"))

(defun semacs--log-command (message)
  "Log command MESSAGE to the command log file."
  (interactive)
  (semacs--log-to-file (concat "[COMMAND] " (message "%s" message)) semacs-log-command-file))

(defun semacs--log-command-success (message)
  "Log command MESSAGE to the command log file."
  (interactive)
  (semacs--log-to-file (concat "[SUCCESS COMMAND] " (message "%s" message)) semacs-log-command-file))

(defun semacs--log-command-error (message)
  "Log command MESSAGE to the command log file."
  (interactive)
  (semacs--log-to-file (concat "[ERROR COMMAND] " (message "%s" message)) semacs-log-command-file))

(defun semacs--log-del-errors ()
  "Delete all error entries from the log file."
  (interactive)
  (let ((log-file semacs-log-file))
    (when (file-exists-p log-file)
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-min))
        (let ((content (buffer-string)))
          (with-temp-file log-file
            (insert (replace-regexp-in-string "^\\[.*?\\] \\[ERROR\\].*\n" "" content))))))))

(defun semacs--log-note (_message)
  "Note MESSAGE to the SEMACS."
  (interactive)
  (semacs--log-message (concat "[NOTE] " (message "%s" _message))))


(defun semacs--get-log (&optional n_head)
  "Read last N_head lines from agent log file using head command. Default is 32 lines."
  (interactive)
  (when (and semacs-log-file (file-exists-p semacs-log-file))
    (condition-case err
        (let ((output (shell-command-to-string
                      (format "head -%d %s" (or n_head 32) semacs-log-file))))
          (when output
            (concat "## Recent Logs\n\n" output)))
      (error
       (semacs--log-message (format "Error reading log: %s" err))
       nil))))

;; (semacs--get-log)

;; (defun semacs--get-log (&optional n_tail)
;;   "Read last N_TAIL lines from agent log file. Default is 32 lines."
;;   (interactive)
;;   (when (and semacs-log-file (file-exists-p semacs-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents semacs-log-file)
;;           (goto-char (point-max))
;;           (forward-line (- (or n_tail 32)))
;;           (when (< (point) (point-min))
;;             (goto-char (point-min)))
;;           (concat "## Recent Logs\n\n"
;;                  (buffer-substring-no-properties (point) (point-max))))
;;       (error
;;        (semacs--log-message (format "Error reading log: %s" err))
;;        nil))))

;; (defun semacs--get-log (&optional n_tail)
;;   "Read last N_TAIL lines from agent log file. Default is 32 lines."
;;   (interactive)
;;   (when (and semacs-log-file (file-exists-p semacs-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents semacs-log-file)
;;           (goto-char (point-max))
;;           (forward-line (- (or n_tail 32)))
;;           (when (< (point) (point-min))
;;             (goto-char (point-min)))
;;           (buffer-substring-no-properties (point) (point-max)))
;;       (error
;;        (semacs--log-message (format "Error reading log: %s" err))
;;        nil))))

;; (defun semacs--get-log (n_tail)
;;   "Read agent log from file."
;;   (interactive)
;;   (when (and semacs-log-file (file-exists-p semacs-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents semacs-log-file)
;;           (buffer-string))
;;       (error (semacs--log-error (format "Error reading log: %s" err))
;;              nil))))


(defun semacs--log-change (file backup changes)
  "Log changes to FILE with BACKUP and CHANGES in standard format."
  (when (and file backup changes)
    (condition-case err
        (with-temp-buffer
          (insert (format "[%s] === Change Log ===\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 (format "File: %s\n" file)
                 (format "Backup: %s\n" backup)
                 (format "Changes:\n%s\n" changes))
          (append-to-file (point-min) (point-max) semacs-log-file))
      (error
       (semacs--log-error
        (format "Failed to log changes: %s" (error-message-string err)))))))

(defun semacs-show-log ()
  "Show agent log using head -f."
  (interactive)
  (when (and semacs-log-file (file-exists-p semacs-log-file))
    (async-shell-command
     (format "head %s" (shell-quote-argument semacs-log-file))
     "*Semacs Log*")))

(defun semacs-stop-log-updates ()
  "Stop log updates."
  (interactive)
  (when-let ((proc (get-buffer-process "*Semacs Log*")))
    (kill-process proc)))



;; (defun semacs--get-log ()
;;   "Read agent log from file."
;;   (when (and semacs-log-file (file-exists-p semacs-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents semacs-log-file)
;;           (buffer-string))
;;       (error (message "Error reading log: %s" err) nil))))

(defun semacs-init-log ()
  "Initialize the log file if it doesn't exist."
  (interactive)
  (unless (file-exists-p semacs-logs-dir)
    (make-directory semacs-logs-dir t))
  (unless (file-exists-p semacs-log-file)
    (with-temp-file semacs-log-file
      (insert (format "=== SEMACS Log Log Created: %s ===\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun semacs-backup-log ()
  "Create a backup of the log file and clear the original."
  (interactive)
  (condition-case err
      (progn
        (semacs-init-log)
        (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
               (backup-dir (expand-file-name "backups" semacs-logs-dir))
               (backup-file (expand-file-name
                           (format "log-%s.log" timestamp)
                           backup-dir)))
          (unless (file-exists-p backup-dir)
            (make-directory backup-dir t))
          (when (file-exists-p semacs-log-file)
            (copy-file semacs-log-file backup-file t)
            (with-temp-file semacs-log-file
              (erase-buffer)
              (insert (format "=== SEMACS Log Log Reset: %s ===\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S"))))
            (semacs--log-message
             (format "Log backed up to %s and cleared" backup-file)))))
    (error
     (semacs--log-message (format "Failed to backup/clear log: %S" err))
     (message "Failed to backup/clear log: %s" err))))


(provide 'semacs-logging)

;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
