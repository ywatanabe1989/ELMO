;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-05 23:28:29
;;; Time-stamp: <2024-12-05 23:28:29 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-logging.el


(require 'ninja-config)
(require 'ninja-server)


(defun ninja--log-message (message)
  "Log MESSAGE to the log file and console."
  (let ((log-file ninja-log-file)
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (when (file-exists-p log-file)
        (insert-file-contents log-file))
      (goto-char (point-min))
      (insert (format "[%s] %s\n" timestamp message))
      (write-region (point-min) (point-max) log-file nil 'quiet))))

(defun ninja--log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (ninja--log-message (concat "[SUCCESS] " (message "%s" _message))))

(defun ninja--log-warning (_message)
  "Log warning MESSAGE."
  (interactive)
  (ninja--log-message (concat "[WARNING] " (message "%s" _message))))

;; ;; show on the client
;; (defun ninja--log-error (_message)
;;   "Log error MESSAGE and run diagnostic command."
;;   (interactive)
;;   (ninja--log-message (concat "[ERROR] " (message "%s" _message)))
;;   (shell-command "head -n 20 ~/.ninja/logs/history.log" "*Error-Log*"))

(defun ninja--log-error (_message)
  "Log error MESSAGE and run diagnostic command."
  (interactive)
  (ninja--log-message (concat "[ERROR] " (message "%s" _message)))
  (ninja-exec-escaped-elisp-code '(shell-command "head -n 20 ~/.ninja/logs/history.log" "*Error-Log*")))


(defun ninja--log-del-errors ()
  "Delete all error entries from the log file."
  (interactive)
  (let ((log-file ninja-log-file))
    (when (file-exists-p log-file)
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-min))
        (let ((content (buffer-string)))
          (with-temp-file log-file
            (insert (replace-regexp-in-string "^\\[.*?\\] \\[ERROR\\].*\n" "" content))))))))

(defun ninja--log-note (_message)
  "Note MESSAGE to the NINJA."
  (interactive)
  (ninja--log-message (concat "[NOTE] " (message "%s" _message))))


(defun ninja--get-log (&optional n_head)
  "Read last N_head lines from agent log file using head command. Default is 32 lines."
  (interactive)
  (when (and ninja-log-file (file-exists-p ninja-log-file))
    (condition-case err
        (let ((output (shell-command-to-string
                      (format "head -%d %s" (or n_head 32) ninja-log-file))))
          (when output
            (concat "## Recent Logs\n\n" output)))
      (error
       (ninja--log-message (format "Error reading log: %s" err))
       nil))))

;; (ninja--get-log)

;; (defun ninja--get-log (&optional n_tail)
;;   "Read last N_TAIL lines from agent log file. Default is 32 lines."
;;   (interactive)
;;   (when (and ninja-log-file (file-exists-p ninja-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents ninja-log-file)
;;           (goto-char (point-max))
;;           (forward-line (- (or n_tail 32)))
;;           (when (< (point) (point-min))
;;             (goto-char (point-min)))
;;           (concat "## Recent Logs\n\n"
;;                  (buffer-substring-no-properties (point) (point-max))))
;;       (error
;;        (ninja--log-message (format "Error reading log: %s" err))
;;        nil))))

;; (defun ninja--get-log (&optional n_tail)
;;   "Read last N_TAIL lines from agent log file. Default is 32 lines."
;;   (interactive)
;;   (when (and ninja-log-file (file-exists-p ninja-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents ninja-log-file)
;;           (goto-char (point-max))
;;           (forward-line (- (or n_tail 32)))
;;           (when (< (point) (point-min))
;;             (goto-char (point-min)))
;;           (buffer-substring-no-properties (point) (point-max)))
;;       (error
;;        (ninja--log-message (format "Error reading log: %s" err))
;;        nil))))

;; (defun ninja--get-log (n_tail)
;;   "Read agent log from file."
;;   (interactive)
;;   (when (and ninja-log-file (file-exists-p ninja-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents ninja-log-file)
;;           (buffer-string))
;;       (error (ninja--log-error (format "Error reading log: %s" err))
;;              nil))))


(defun ninja--log-change (file backup changes)
  "Log changes to FILE with BACKUP and CHANGES in standard format."
  (when (and file backup changes)
    (condition-case err
        (with-temp-buffer
          (insert (format "[%s] === Change Log ===\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S"))
                 (format "File: %s\n" file)
                 (format "Backup: %s\n" backup)
                 (format "Changes:\n%s\n" changes))
          (append-to-file (point-min) (point-max) ninja-log-file))
      (error
       (ninja--log-error
        (format "Failed to log changes: %s" (error-message-string err)))))))

(defun ninja-show-log ()
  "Show agent log using head -f."
  (interactive)
  (when (and ninja-log-file (file-exists-p ninja-log-file))
    (async-shell-command
     (format "head %s" (shell-quote-argument ninja-log-file))
     "*Ninja Log*")))

(defun ninja-stop-log-updates ()
  "Stop log updates."
  (interactive)
  (when-let ((proc (get-buffer-process "*Ninja Log*")))
    (kill-process proc)))



;; (defun ninja--get-log ()
;;   "Read agent log from file."
;;   (when (and ninja-log-file (file-exists-p ninja-log-file))
;;     (condition-case err
;;         (with-temp-buffer
;;           (insert-file-contents ninja-log-file)
;;           (buffer-string))
;;       (error (message "Error reading log: %s" err) nil))))

(defun ninja-init-log ()
  "Initialize the log file if it doesn't exist."
  (interactive)
  (unless (file-exists-p ninja-logs-dir)
    (make-directory ninja-logs-dir t))
  (unless (file-exists-p ninja-log-file)
    (with-temp-file ninja-log-file
      (insert (format "=== NINJA Log Log Created: %s ===\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun ninja-backup-log ()
  "Create a backup of the log file and clear the original."
  (interactive)
  (condition-case err
      (progn
        (ninja-init-log)
        (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
               (backup-dir (expand-file-name "backups" ninja-logs-dir))
               (backup-file (expand-file-name
                           (format "log-%s.log" timestamp)
                           backup-dir)))
          (unless (file-exists-p backup-dir)
            (make-directory backup-dir t))
          (when (file-exists-p ninja-log-file)
            (copy-file ninja-log-file backup-file t)
            (with-temp-file ninja-log-file
              (erase-buffer)
              (insert (format "=== NINJA Log Log Reset: %s ===\n\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S"))))
            (ninja--log-message
             (format "Log backed up to %s and cleared" backup-file)))))
    (error
     (ninja--log-message (format "Failed to backup/clear log: %S" err))
     (message "Failed to backup/clear log: %s" err))))


(provide 'ninja-logging)

;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
