;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 00:55:34
;;; Time-stamp: <2025-01-03 00:55:34 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/file-variables.el

(defcustom llemacs--logging-level-threshold 'info
  "Minimum log level to record."
  :type 'symbol
  :group 'llemacs-logging)

(defcustom llemacs--logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs--logging-backup-count 5
  "Number of log backup files to keep."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs--logging-splitter "----------------------------------------"
  "Splitter between logs."
  :type 'string
  :group 'llemacs-logging)

(defun llemacs--logging-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info 1)
    ('success 1)
    ('search 1)
    ('api 1)
    ('prompt 1)
    ('elisp 1)
    ('warn 2)
    ('success 3)
    ('error 4)
    (_ 1)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))