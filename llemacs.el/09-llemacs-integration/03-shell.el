;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:07:49
;;; Time-stamp: <2025-01-06 20:07:49 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/01-shell.el

;; Shell integration
(defun llemacs--run-shell-command (command)
  (llemacs--logging-write-info-pj
   (format "Executing shell command: %s" command))
  (condition-case err
      (progn
        (shell-command command)
        (llemacs--logging-write-success-pj
         (format "Shell command completed: %s" command)))
    (error
     (llemacs--logging-write-error-pj
      (format "Shell command failed: %s - %s" command err)))))
