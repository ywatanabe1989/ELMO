;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 09:01:57
;;; Time-stamp: <2025-01-04 09:01:57 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/level.el

(defun llemacs--logging-get-level-value (level)
  "Get numeric value for log LEVEL."
  (car (cdr (assq level llemacs--log-levels-sys))))

(defun llemacs--logging-should-log-p (level)
  "Check if LEVEL should be logged based on threshold."
  (>= (llemacs--logging-get-level-value level)
      (llemacs--logging-get-level-value llemacs--logging-level-threshold)))

;; (defun llemacs--logging-log (level message &optional project-id-or-full-name)
;;   "Log MESSAGE at LEVEL with optional PROJECT-ID-OR-FULL-NAME."
;;   (when (llemacs--logging-should-log-p level)
;;     (let* ((log-entry (llemacs--logging-format-message level message project-id-or-full-name))
;;            (log-file
;;             (if project-id-or-full-name
;;                 (let ((level-var (intern (format "llemacs--path-pj-logs-%s" level))))
;;                   (symbol-value level-var))
;;               (let ((level-var (intern (format "llemacs--path-logs-%s-sys" level))))
;;                 (symbol-value level-var))))
;;            (all-log-file (if project-id-or-full-name
;;                              llemacs--path-pj-logs-all
;;                            llemacs--path-logs-all-sys)))
;;       (llemacs--logging-system-files-init-if)
;;       (append-to-file (concat log-entry "\n") nil all-log-file)
;;       (append-to-file (concat log-entry "\n") nil log-file))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))