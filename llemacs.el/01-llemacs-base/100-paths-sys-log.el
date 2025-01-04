;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 10:44:17
;;; Time-stamp: <2025-01-04 10:44:17 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/100-paths-sys-log.el

;; Logs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-logs-sys
  (expand-file-name "logs" llemacs--path-workspace)
  "Directory for LLEMACS system logs."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-backup-sys
  (expand-file-name "backup" llemacs--path-logs-sys)
  "Directory for LLEMACS system logs backup."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-all-sys
  (expand-file-name "all" llemacs--path-logs-sys)
  "File path for LLEMACS system logs."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-by-level-sys
  (expand-file-name "by_level" llemacs--path-logs-sys)
  "Directory for LLEMACS system logs."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defconst llemacs--log-levels-sys
  '((debug   . (0 . "Debug level logs"))
    (info    . (1 . "Information level logs"))
    (success . (1 . "Success level logs"))
    (prompt  . (1 . "Prompt operation logs"))
    (elisp   . (1 . "Elisp execution logs"))
    (api     . (1 . "API interaction logs"))
    (search  . (1 . "Search operation logs"))
    (warn    . (2 . "Warning level logs"))
    (error   . (3 . "Error level logs"))))

(defun llemacs--path-create-log-paths-sys ()
  "Create system-level log path variables based on `llemacs--log-levels-sys'.
The following variables are initialized in default:
- `llemacs--path-logs-debug-sys'
- `llemacs--path-logs-info-sys'
- `llemacs--path-logs-success-sys'
- `llemacs--path-logs-prompt-sys'
- `llemacs--path-logs-elisp-sys'
- `llemacs--path-logs-api-sys'
- `llemacs--path-logs-search-sys'
- `llemacs--path-logs-warn-sys'
- `llemacs--path-logs-error-sys'"

  (dolist (level llemacs--log-levels-sys)
    (let* ((level-name (car level))
           (level-info (cdr level))
           (desc (cdr level-info))
           (var-name (intern (format "llemacs--path-logs-%s-sys" level-name))))
      (eval
       `(defcustom ,var-name
          (expand-file-name ,(format "%s.log" level-name) llemacs--path-logs-by-level-sys)
          ,(format "Path to LLEMACS system %s file." desc)
          :type 'file
          :group 'llemacs-path
          :group 'llemacs-logging
          :group 'llemacs-sys)))))

(llemacs--path-create-log-paths-sys)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))