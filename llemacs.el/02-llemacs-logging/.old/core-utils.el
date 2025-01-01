;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 05:50:48
;;; Time-stamp: <2025-01-01 05:50:48 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/core/02-llemacs-logging-core-utils.el

;; (require '01-llemacs-config)



;; (defun llemacs--logging-timestamp ()
;;   "Get current timestamp in ISO 8601 format."
;;   (format-time-string "%Y-%m-%dT%H:%M:%S"))

;; (defun llemacs--logging-project-parent ()
;;   "Get current project's parent ID or empty string."
;;   (or (and (boundp 'llemacs--current-project-parent)
;;            llemacs--current-project-parent)
;;       ""))

;; (defun llemacs--logging-task-parent ()
;;   "Get current task's parent ID or empty string."
;;   (or (and (boundp 'llemacs--current-task-parent)
;;            llemacs--current-task-parent)
;;       ""))

(provide '02-llemacs-logging-core-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))