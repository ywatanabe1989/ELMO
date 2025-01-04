;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
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