;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:26:34
;;; Time-stamp: <2024-12-31 17:26:34 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/core/file.el

(require '01-llemacs-config)

(defvar llemacs--logging-level-threshold 'info
  "Minimum log level to record.")

(defcustom llemacs--logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs--logging-backup-count 5
  "Number of log backup files to keep."
  :type 'integer
  :group 'llemacs-logging)

(defun llemacs--logging-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info 1)
    ('search 1)
    ('api 1)
    ('prompt 1)
    ('warn 2)
    ('success 3)
    ('error 4)
    (_ 1)))

(provide '02-llemacs-logging-core-file)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))