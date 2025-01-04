;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 20:56:45
;;; Time-stamp: <2025-01-03 20:56:45 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/004-timestamp.el

(defcustom llemacs--timestamp
  (format-time-string "%Y-%m-%d-%H:%M:%S")
  "Timestamp references by LLEMACS."
  :type 'string
  :group 'llemacs-sys)

(defun llemacs-timestamp-get ()
  "Get current timestamp without updating the reference."
  (format-time-string "%Y-%m-%d-%H:%M:%S"))

(defalias 'llemacs-timestamp 'llemacs-timestamp-get)

(defun llemacs--timestamp-set ()
  "Set the reference timestamp."
  (setq llemacs--timestamp (llemacs-timestamp-get)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))