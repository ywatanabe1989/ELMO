;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 11:09:56
;;; Timestamp: <2025-01-10 11:09:56>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/01-run-elisp.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defvar llemacs--run-start-tag
  "
================================================================================
Llemacs-run called.
================================================================================"
  "Starting tag for Llemacs run")

;; ;; org is produced but pdf is not
;; (defun llemacs--ensure-elisp-code (code)
;;   "Ensure CODE is in proper format for evaluation."
;;   (condition-case err
;;       (cond
;;        ((stringp code)
;;         (read code))
;;        ((and (listp code)
;;              (eq (car code) 'progn))
;;         code)  ; Keep the entire progn structure
;;        ((listp code)
;;         code)
;;        (t
;;         (llemacs--logging-write-error-pj "Invalid code format")))
;;     (llemacs--logging-write-error-pj
;;      (llemacs--logging-write-error-pj "Code validation failed: %s" (error-message-string err)))))

(defun llemacs--ensure-elisp-code (code)
  "Ensure CODE is in proper format for evaluation."
  (condition-case err
      (cond
       ((stringp code)
        (read code))
       ((listp code)
        code)
       (t
        (llemacs--logging-write-error-pj "Invalid code format")))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj "Code validation failed: %s" (error-message-string err)))))

;; (defun llemacs--escape-elisp-code (code)
;;   "Escape elisp CODE for shell execution."
;;   (condition-case err
;;       (prin1-to-string code)
;;     (llemacs--logging-write-error-pj
;;      (llemacs--logging-write-error-pj "Code escaping failed: %s" (error-message-string err)))))

;; ----------------------------------------
;; Runners
;; ----------------------------------------

;; ;; org is produced but pdf is not
;; (defun llemacs--run-elisp-local (elisp-code)
;;   "Run elisp code in local Emacs."
;;   (llemacs--timestamp-set)
;;   (let ((gc-cons-threshold (* 100 1024 1024)))
;;     (condition-case err
;;         (eval (llemacs--ensure-elisp-code elisp-code) t)
;;       (llemacs--logging-write-error-pj
;;        (llemacs--logging-write-error-pj "llemacs--run-elisp-local failed: %s" (error-message-string err))))))

(defun llemacs--run-elisp-local (elisp-code)
  "Run elisp code in local Emacs."
  (llemacs--timestamp-set)
  (let ((gc-cons-threshold (* 100 1024 1024)))
    (condition-case err
        ;; (sleep-for 0.3)
      (eval elisp-code t)
      ;; (sleep-for 0.3)
      (llemacs--logging-write-error-pj
       (llemacs--logging-write-error-pj "llemacs--run-elisp-local failed: %s" (error-message-string err))))))

(defun llemacs--run-elisp-server (elisp-code &optional emacs-server-file)
  "Run elisp code in remote Emacs server specified by EMACS-SERVER-FILE."
  (llemacs--timestamp-set)
  (condition-case err
      (let* ((server-file (or emacs-server-file llemacs--path-agent-emacs-server))
             (code-to-send (prin1-to-string (llemacs--ensure-elisp-code elisp-code))))
        (unless (file-exists-p server-file)
          (llemacs--logging-write-error-pj "Emacs server file not found: %s" server-file))
        (unless (file-readable-p server-file)
          (llemacs--logging-write-error-pj "No read permission for server file: %s" server-file))
        (let ((cmd (format "emacsclient -s %s -e %s"
                           (shell-quote-argument server-file)
                           (shell-quote-argument code-to-send))))
          (shell-command cmd)))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj "Server code execution failed: %s" (error-message-string err)))))

;; (llemacs--run-elisp-local "(message \"aaa\")") ;; working
;; (llemacs--run-elisp-local '(message "aaa")) ;; working

;; (defun llemacs--run-elisp (elisp-code)
;;   "Run elisp code, trying server first then falling back to local."
;;   (interactive)
;;   (llemacs--timestamp-set)
;;   (condition-case err
;;       (let ((code (if (stringp elisp-code)
;;                       (read elisp-code)
;;                     elisp-code)))
;;         (if (and llemacs--path-agent-emacs-server
;;                  (file-exists-p llemacs--path-agent-emacs-server)
;;                  (file-readable-p llemacs--path-agent-emacs-server))
;;             (llemacs--run-elisp-server code llemacs--path-agent-emacs-server)
;;           (llemacs--run-elisp-local code)))
;;     (llemacs--logging-write-error-pj
;;      (llemacs--logging-write-error-pj "Code execution failed: %s" (error-message-string err)))))

(defun llemacs--run-elisp (elisp-code)
  (llemacs--update-docs)
  (condition-case-unless-debug err
      (let ((code (if (stringp elisp-code)
                      (read elisp-code)
                    elisp-code)))
        (if (and llemacs--path-agent-emacs-server
                 (file-exists-p llemacs--path-agent-emacs-server)
                 (file-readable-p llemacs--path-agent-emacs-server))
            (llemacs--run-elisp-server code llemacs--path-agent-emacs-server)
          (llemacs--run-elisp-local code)))
    ((void-function void-variable error)
     (llemacs--logging-write-error-pj
      "Code execution failed: %s"
      (error-message-string err)))))

;; (llemacs--run-elisp '(message "hi")) ;; working
;; (llemacs--run-elisp "(message \"hi\")") ;; working
;; (llemacs--run-prompt "plot something" "code-elisp-progn") ;; not working
;; (llemacs--run-prompt "plot five figures for digital signal processing" "code-elisp-progn") ;; not working


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
