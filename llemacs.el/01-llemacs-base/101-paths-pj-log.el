;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 10:50:43
;;; Time-stamp: <2025-01-04 10:50:43 (ywatanabe)>
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

(defun llemacs--path-create-log-paths-pj ()
  "Create project-specific log path variables based on `llemacs--log-levels-pj'.
The following variables and their aliases are created in default:
Original variables:
- `llemacs--path-pj-logs-debug'
- `llemacs--path-pj-logs-info'
- `llemacs--path-pj-logs-success'
- `llemacs--path-pj-logs-prompt'
- `llemacs--path-pj-logs-elisp'
- `llemacs--path-pj-logs-api'
- `llemacs--path-pj-logs-search'
- `llemacs--path-pj-logs-warn'
- `llemacs--path-pj-logs-error'

Aliased as:
- `llemacs--path-logs-debug-pj'
- `llemacs--path-logs-info-pj'
- `llemacs--path-logs-success-pj'
- `llemacs--path-logs-prompt-pj'
- `llemacs--path-logs-elisp-pj'
- `llemacs--path-logs-api-pj'
- `llemacs--path-logs-search-pj'
- `llemacs--path-logs-warn-pj'
- `llemacs--path-logs-error-pj'"
  (unless llemacs--cur-pj
    (error "No project selected"))
  (dolist (level llemacs--log-levels-pj)
    (let* ((level-name (car level))
           (var-name (intern (format "llemacs--path-pj-logs-%s" level-name)))
           (alias-name (intern (format "llemacs--path-logs-%s-pj" level-name)))
           (path (expand-file-name (format "%s.log" level-name)
                                   (symbol-value 'llemacs--path-pj-logs-by-level))))
      (set var-name path)
      (defalias alias-name var-name))))

(llemacs--path-create-log-paths-pj)

;; (llemacs-list "variable" "^llemacs--path")
;; (llemacs-list "variable" "^llemacs--path-logs")
;; (llemacs-list "variable" "^llemacs--path-pj")

(defun llemacs--path-pj-ensure-all ()
  "Ensure all project directories and log files exist."
  (when (and llemacs--cur-pj llemacs--path-pj)
    (let ((pj-id (llemacs--cur-pj-get)))
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