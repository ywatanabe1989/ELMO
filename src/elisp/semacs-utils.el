;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:14:11
;;; Time-stamp: <2024-12-04 08:14:11 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-utils.el


;;; Commentary:
;; Utility functions for self-evolving agent

;;; Code:
(defun semacs--sudo-get-password ()
  "Get sudo password once and store it."
  (interactive)
  (unless semacs--sudo-password
    (setq semacs--sudo-get-password (read-passwd "Sudo password: ")))
  semacs--sudo-password)

(defun semacs--shell-command (command)
  "Execute shell COMMAND and return output or nil on error."
  (condition-case err
      (with-temp-buffer
        (let ((exit-code (call-process-shell-command command nil t)))
          (if (zerop exit-code)
              (buffer-string)
            (error "Command failed with exit code %d: %s" exit-code command))))
    (error (message "Shell command error: %s" err) nil)))

(defun semacs--diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (or (semacs--shell-command (format "diff -u %s %s" file1 file2))
      "No differences found"))

(defun semacs--show-progress (message)
  "Show progress MESSAGE in dedicated window."
  (let ((buffer (get-buffer-create "*semacs-progress*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (format "[%s] %s\n"
                       (format-time-string "%H:%M:%S")
                       message)))
      (display-buffer buffer))))


(defun semacs--create-backup (file)
  "Create backup of FILE with timestamp."
  (when (and file (file-exists-p file))
    (let* ((base (file-name-sans-extension file))
           (ext (file-name-extension file))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (backup-name (format "%s-%s.%s"
                              (file-name-nondirectory base)
                              timestamp
                              ext))
           (backup-path (expand-file-name backup-name semacs-backups-dir)))
      (condition-case err
          (progn
            (make-directory semacs-backups-dir t)
            (copy-file file backup-path t)
            backup-path)
        (error (message "Backup failed for %s: %s" file err) nil)))))


(defun semacs--update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-semacsrch-forward "Time-stamp: <.*>" nil t)
      (let ((new-timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
        (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                             new-timestamp))))))

(provide 'semacs-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
