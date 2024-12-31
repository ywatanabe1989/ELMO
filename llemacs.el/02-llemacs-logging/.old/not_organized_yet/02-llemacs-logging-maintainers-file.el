;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:01:12
;;; Time-stamp: <2024-12-31 17:01:12 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-maintainers-file.el


(require '02-llemacs-logging-db)

(defun llemacs--logging-cleanup-old-backups (days)
  "Remove log backup files older than DAYS."
  (let* ((cutoff (time-subtract (current-time) (days-to-time days)))
         (backup-files (directory-files llemacs--path-log-backups t "\\.log-.*")))
    (dolist (file backup-files)
      (when (time-less-p (nth 5 (file-attributes file)) cutoff)
        (delete-file file)))))

(defun llemacs--logging-archive-logs (archive-dir)
  "Archive current logs to ARCHIVE-DIR with timestamp."
  (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
    (make-directory archive-dir t)
    (copy-file llemacs--path-log-system
               (expand-file-name (format "system-%s.log" timestamp) archive-dir))))

(defun llemacs--logging-truncate-log-file (max-size)
  "Truncate log file to MAX-SIZE bytes, keeping most recent entries."
  (when (file-exists-p llemacs--path-log-system)
    (with-temp-buffer
      (insert-file-contents llemacs--path-log-system)
      (when (> (buffer-size) max-size)
        (delete-region (point-min)
                       (- (point-max) max-size))
        (write-region (point-min) (point-max)
                      llemacs--path-log-system)))))

(provide '02-llemacs-logging-maintainers-file)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))