;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 20:05:51
;;; Timestamp: <2025-01-09 20:05:51>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/06-run-step.el

(defun llemacs-run-step ()
  "Process one step for the current project."
  (llemacs--run-update-project-management)
  (llemacs--run-advance-project))

;; (llemacs-run-step)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
