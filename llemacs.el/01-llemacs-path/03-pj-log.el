;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 09:21:48
;;; Time-stamp: <2025-01-06 09:21:48 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/101-paths-pj-log.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--path-pj-logs
  (expand-file-name "logs" llemacs--path-pj)
  "Directory for current project logs."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-backup
  (expand-file-name "backup" llemacs--path-pj-logs)
  "Directory for current project logs backup."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-all
  (expand-file-name "all.log" llemacs--path-pj-logs)
  "File for current project logs."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-by-level
  (expand-file-name "by_level" llemacs--path-pj-logs)
  "Directory for current project logs."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defconst llemacs--log-levels-pj
  llemacs--log-levels-sys
  "Project-level logging-levels")

(defalias 'llemacs--path-logs-pj 'llemacs--path-pj-logs)
(defalias 'llemacs--path-logs-backup-pj 'llemacs--path-pj-logs-backup)
(defalias 'llemacs--path-logs-all-pj 'llemacs--path-pj-logs-all)
(defalias 'llemacs--path-logs-by-level-pj 'llemacs--path-pj-logs-by-level)

;; (defun llemacs--path-define-log-paths-pj ()
;;   (dolist (level llemacs--log-levels-pj)
;;     (let* ((level-name (car level))
;;            (level-info (cdr level))
;;            (level-desc (cdr level-info))
;;            (var-name-1 (intern (format "llemacs--path-pj-logs-%s" level-name)))
;;            (var-name-2 (intern (format "llemacs--path-logs-%s-pj" level-name))))
;;       (eval `(defvar ,var-name-1 nil
;;                ,(format "Path to %s log file for current project. (= `%s')\nSee `llemacs--path-logs-update-pj'."
;;                         level-name var-name-2)))
;;       (eval `(defvar ,var-name-2 nil
;;                ,(format "Path to %s log file for current project (= `%s').\nSee `llemacs--path-logs-update-pj'."
;;                         level-name var-name-1)))))
;;   )

(defun llemacs--path-define-log-paths-pj ()
  (let ((msg-base "Log file paths for %s for current project. (= `%s')\nSee `llemacs--path-logs-update-pj'."))
    (dolist (level llemacs--log-levels-pj)
      (let* ((level-name (car level))
             (level-info (cdr level))
             (level-desc (cdr level-info))
             (var-name-1 (intern (format "llemacs--path-pj-logs-%s" level-name)))
             (var-name-2 (intern (format "llemacs--path-logs-%s-pj" level-name))))
        (eval `(defvar ,var-name-1 nil
                 ,(format msg-base level-name var-name-2)))
        (eval `(defvar ,var-name-2 nil
                 ,(format msg-base level-name var-name-1)))))))

(defun llemacs--path-logs-update-pj ()
  "Creates project-specific log path variables

`llemacs--path-pj-logs-debug' (= `llemacs--path-logs-debug-pj')
`llemacs--path-pj-logs-info' (= `llemacs--path-logs-info-pj')
`llemacs--path-pj-logs-success' (= `llemacs--path-logs-success-pj')
`llemacs--path-pj-logs-prompt' (= `llemacs--path-logs-prompt-pj')
`llemacs--path-pj-logs-elisp' (= `llemacs--path-logs-elisp-pj')
`llemacs--path-pj-logs-api' (= `llemacs--path-logs-api-pj')
`llemacs--path-pj-logs-search' (= `llemacs--path-logs-search-pj')
`llemacs--path-pj-logs-warn' (= `llemacs--path-logs-warn-pj')
`llemacs--path-pj-logs-errror' (= `llemacs--path-logs-errror-pj')

The path values use `llemacs--path-pj-logs-by-level' as the base directory.

See also:
- `llemacs--log-levels-pj'
- `llemacs--path-pj-logs-by-level'
- `llemacs--cur-pj'

Signals error if no project is selected."
  (llemacs--path-define-log-paths-pj)
  (unless llemacs--cur-pj
    (error "No project selected"))
  (dolist (level llemacs--log-levels-pj)
    (let* ((level-name (car level))
           (level-info (cdr level))
           (level-desc (cdr level-info))
           (var-name-1 (intern (format "llemacs--path-pj-logs-%s" level-name)))
           (var-name-2 (intern (format "llemacs--path-logs-%s-pj" level-name)))
           (path (expand-file-name (format "%s.log" level-name)
                                   (symbol-value 'llemacs--path-pj-logs-by-level))))
      (set var-name-1 path)
      (set var-name-2 path))))

(llemacs--path-logs-update-pj)

;; (defun llemacs--path-pj-ensure-all ()
;;   "Ensure all project directories and log files exist."
;;   (when (and llemacs--cur-pj llemacs--path-pj)
;;     (let ((pj-id (llemacs--pj-get-cur-pj)))
;;       ;; Ensure directories
;;       (dolist (dir (list llemacs--path-pj
;;                          llemacs--path-pj-config
;;                          llemacs--path-pj-logs
;;                          llemacs--path-pj-logs-by-level))
;;         (unless (file-exists-p dir)
;;           (make-directory dir t)))

;;       ;; Create log files
;;       (dolist (level-config llemacs--log-levels-pj)
;;         (let* ((level (car level-config))
;;                (log-file (symbol-value (intern (format "llemacs--path-pj-logs-%s" level)))))
;;           (unless (file-exists-p log-file)
;;             (with-temp-buffer
;;               (write-file log-file)))))

;;       ;; Create all.log
;;       (unless (file-exists-p llemacs--path-pj-logs-all)
;;         (with-temp-buffer
;;           (write-file llemacs--path-pj-logs-all))))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))