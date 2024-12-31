;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:06:38
;;; Time-stamp: <2024-12-31 23:06:38 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base/01-llemacs-base-timestamp.el

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