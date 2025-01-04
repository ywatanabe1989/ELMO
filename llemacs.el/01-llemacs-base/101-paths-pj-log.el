;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 16:34:23
;;; Time-stamp: <2025-01-04 16:34:23 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/101-paths-pj-log.el

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

(defalias 'llemacs--path-logs-pj 'llemacs--path-pj-logs)
(defalias 'llemacs--path-logs-backup-pj 'llemacs--path-pj-logs-backup)
(defalias 'llemacs--path-logs-all-pj 'llemacs--path-pj-logs-all)
(defalias 'llemacs--path-logs-by-level-pj 'llemacs--path-pj-logs-by-level)

(defconst llemacs--log-levels-pj llemacs--log-levels-sys)


(defun llemacs--path-init-log-paths-pj ()
  ;; First define all variables
  (dolist (level llemacs--log-levels-pj)
    (let* ((level-name (car level))
           (level-info (cdr level))
           (level-desc (cdr level-info))
           (var-name-1 (intern (format "llemacs--path-pj-logs-%s" level-name)))
           (var-name-2 (intern (format "llemacs--path-logs-%s-pj" level-name))))
      (eval `(defvar ,var-name-1 nil
               ,(format "Path to %s log file for current project. (= `%s')\nSee `llemacs--path-create-or-update-log-paths-pj'."
                        level-name var-name-2)))
      (eval `(defvar ,var-name-2 nil
               ,(format "Path to %s log file for current project (= `%s').\nSee `llemacs--path-create-or-update-log-paths-pj'."
                        level-name var-name-1)))))
  )

(defun llemacs--path-create-or-update-log-paths-pj ()
  "Create and alias project-specific log path variables.

This function creates project-specific log path variables and their corresponding
aliases based on `llemacs--log-levels-pj' configuration. The variables
follow a consistent naming pattern:

Original variables are named as:
\\[llemacs--path-pj-logs-LEVEL] where LEVEL is one of:
- debug: Debug level logs
- info: Information level logs
- success: Success level logs
- prompt: Prompt operation logs
- elisp: Elisp execution logs
- api: API interaction logs
- search: Search operation logs
- warn: Warning level logs
- error: Error level logs

Each original variable has a corresponding alias named:
\\[llemacs--path-logs-LEVEL-pj]

For example:
\\[llemacs--path-pj-logs-debug] -> \\[llemacs--path-logs-debug-pj]

The path values use `llemacs--path-pj-logs-by-level' as the base directory.

See also:
- `llemacs--log-levels-pj'
- `llemacs--path-pj-logs-by-level'
- `llemacs--cur-pj'

Signals error if no project is selected."

  (llemacs--path-init-log-paths-pj)

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
      (set var-name-2 path)
      )))

(llemacs--path-create-or-update-log-paths-pj)

;; llemacs--path-pj-logs-error
;; llemacs--path-logs-error-pj


;; (llemacs-list "variable" "^llemacs--path")
;; (llemacs-list "variable" "^llemacs--path-logs")
;; (llemacs-list "variable" "^llemacs--path-pj")

(defun llemacs--path-pj-ensure-all ()
  "Ensure all project directories and log files exist."
  (when (and llemacs--cur-pj llemacs--path-pj)
    (let ((pj-id (llemacs--pj-get-cur-pj)))
      ;; Ensure directories
      (dolist (dir (list llemacs--path-pj
                         llemacs--path-pj-config
                         llemacs--path-pj-logs
                         llemacs--path-pj-logs-by-level))
        (unless (file-exists-p dir)
          (make-directory dir t)))

      ;; Create log files
      (dolist (level-config llemacs--log-levels-pj)
        (let* ((level (car level-config))
               (log-file (symbol-value (intern (format "llemacs--path-pj-logs-%s" level)))))
          (unless (file-exists-p log-file)
            (with-temp-buffer
              (write-file log-file)))))

      ;; Create all.log
      (unless (file-exists-p llemacs--path-pj-logs-all)
        (with-temp-buffer
          (write-file llemacs--path-pj-logs-all))))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))