;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 23:13:13
;;; Time-stamp: <2024-12-24 23:13:13 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/03-elmo-logging-utils.el

(require '01-elmo-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils for Log File Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elmo-backup-log ()
  "Create a backup of the log file and clear the original."
  (interactive)
  (condition-case err
      (progn
        (elmo-init-log)
        (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
               (backup-dir (expand-file-name "backups" elmo-logs-dir))
               (backup-file (expand-file-name
                             (format "log-%s.log" timestamp)
                             backup-dir)))
          (unless (file-exists-p backup-dir)
            (make-directory backup-dir t))
          (when (file-exists-p elmo-log-file)
            (copy-file elmo-log-file backup-file t)
            (with-temp-file elmo-log-file
              (erase-buffer)
              (insert (format "=== ELMO Log Log Reset: %s ===\n\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S"))))
            (elmo-log-message
             (format "Log backed up to %s and cleared" backup-file)))))
    (error
     (elmo-log-message (format "Failed to backup/clear log: %S" err))
     (message "Failed to backup/clear log: %s" err))))

(defun elmo-log-del-errors ()
  "Delete all error entries from the log file."
  (interactive)
  (let ((log-file elmo-log-file))
    (when (file-exists-p log-file)
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-min))
        (let ((content (buffer-string)))
          (with-temp-file log-file
            (insert (replace-regexp-in-string "^\\[.*?\\] \\[ERROR\\].*\n" "" content))))))))

(defun elmo-log-change (file backup changes)
  "Log changes to FILE with BACKUP and CHANGES in standard format."
  (when (and file backup changes)
    (condition-case err
        (with-temp-buffer
          (insert (format "[%s] === Change Log ===\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S"))
                  (format "File: %s\n" file)
                  (format "Backup: %s\n" backup)
                  (format "Changes:\n%s\n" changes))
          (append-to-file (point-min) (point-max) elmo-log-file))
      (error
       (elmo-log-error
        (format "Failed to log changes: %s" (error-message-string err)))))))

(defun elmo-stop-log-updates ()
  "Stop log updates."
  (interactive)
  (when-let ((proc (get-buffer-process "*ELMO Log*")))
    (kill-process proc)))

(provide '03-elmo-logging-utils)

                                        ;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))