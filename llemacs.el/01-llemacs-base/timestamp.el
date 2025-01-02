;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:49:58
;;; Time-stamp: <2025-01-02 10:49:58 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/timestamp.el

(defcustom llemacs--timestamp
  (format-time-string "%Y-%m-%d-%H:%M:%S")
  "Timestamp references by LLEMACS"
  :type 'string
  :group 'llemacs)

(defun llemacs--timestamp-get ()
  "Get current timestamp without updating the reference."
  (format-time-string "%Y-%m-%d-%H:%M:%S"))

(defun llemacs-timestamp-update ()
  "Updates the reference timestamp."
  (setq llemacs--timestamp (llemacs--timestamp-get)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))