;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:24:42
;;; Time-stamp: <2025-01-06 17:24:42 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/refreshers.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--logging-refresh-interval 5
  "Interval in seconds for auto-refreshing log buffer."
  :type 'integer
  :group 'llemacs-logging)

(defun llemacs--logging-backup-files ()
  "Backup all log files with timestamp."
  (interactive)
  (message "backup-files called")
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S")) ; Updated timestamp format
         (backup-dir (expand-file-name timestamp llemacs--path-logs-backup-sys)))
    (unless (file-exists-p backup-dir)
      (make-directory backup-dir t))
    (dolist (level (mapcar #'car llemacs--log-levels-sys))
      (let* ((sys-file (symbol-value (intern (format "llemacs--path-logs-%s-sys" level))))
             (backup-file (expand-file-name
                           (format "%s-%s.log" level timestamp)
                           backup-dir)))
        (when (file-exists-p sys-file)
          (copy-file sys-file backup-file t))))))

(defun llemacs--logging-refresh-files ()
  "Refresh all log files after backing up."
  (interactive)
  (message "refresh-files called")
  (llemacs--logging-backup-files)
  (dolist (level (mapcar #'car llemacs--log-levels-pj))
    (let ((sys-file (symbol-value (intern (format "llemacs--path-logs-%s-pj" level)))))
      (when (file-exists-p sys-file)
        (write-region "" nil sys-file)))))

(defun llemacs--logging-refresh-buffer ()
  "Refresh the current log buffer."
  (interactive)
  (message "llemacs--logging-refresh-buffer fixme"))
;; (when (string= (buffer-name) llemacs--buf-logging-main-pj)
;;   (let ((current-point (point)))
;;     (if level
;;         (llemacs--logging-view-logs-by-level (symbol-name level))
;;       (llemacs--logging-view-all-logs)))
;;   (llemacs--logging-view-all-logs))
;; (goto-char current-point))))

(defun llemacs--logging-refresh ()
  "Refresh both log files and buffer."
  (interactive)
  (message "refresh called")
  (llemacs--logging-refresh-files)
  (llemacs--logging-refresh-buffer))

(defun llemacs--logging-auto-refresh-mode ()
  "Toggle auto-refresh for log buffer."
  (interactive)
  (if (timerp llemacs--logging-refresh-timer)
      (progn
        (cancel-timer llemacs--logging-refresh-timer)
        (setq llemacs--logging-refresh-timer nil)
        (message "Auto-refresh disabled"))
    (setq llemacs--logging-refresh-timer
          (run-with-timer 0 llemacs--logging-refresh-interval #'llemacs--logging-refresh))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
