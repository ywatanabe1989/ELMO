;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:24:02
;;; Time-stamp: <2025-01-02 10:24:02 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-elisp.el

(defvar llemacs--run-start-tag
  "
================================================================================
Llemacs-run called.
================================================================================"
  "Starting tag for Llemacs run")

(defun llemacs--escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (prin1-to-string code)
    (error
     (llemacs--logging-write-error-pj (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

;; ----------------------------------------
;; Runners
;; ----------------------------------------
(defun llemacs--run-elisp-local (escaped-elisp-code)
  "Run elisp code in local Emacs."
  (condition-case err
      (eval escaped-elisp-code)
    (error
     (llemacs--logging-write-error-pj (format "Local code runution failed: %s" (error-message-string err)))
     nil)))

(defun llemacs--run-elisp-server (escaped-elisp-code &optional emacs-server-file)
  "Run elisp code in remote Emacs server specified by EMACS-SERVER-FILE."
  (condition-case err
      (let* ((server-file (or emacs-server-file llemacs--path-agent-emacs-server))
             (escaped-code (prin1-to-string escaped-elisp-code)))
        ;; Check if server exists and is running
        (unless (file-exists-p server-file)
          (error "Emacs server file not found: %s" server-file))
        ;; Check server file permissions
        (unless (file-readable-p server-file)
          (error "No read permission for server file: %s" server-file))
        (let ((cmd (format "emacsclient -s %s -e %s"
                           (shell-quote-argument server-file)
                           (shell-quote-argument escaped-code))))
          (shell-command cmd)))
    (error
     (llemacs--logging-write-error-pj (format "Server code runution failed: %s" (error-message-string err)))
     nil)))

(defun llemacs--run-elisp (escaped-elisp-code)
  "Run elisp code, trying server first then falling back to local."
  (interactive)
  (condition-case err
      (let* ((server-file (or llemacs--path-agent-emacs-server nil)))
        (if (and server-file
                 (file-exists-p server-file)
                 (file-readable-p server-file))
            ;; Try server first
            (llemacs--run-elisp-server escaped-elisp-code server-file)
          ;; Fall back to local if server unavailable
          (llemacs--run-elisp-local escaped-elisp-code)))
    (error
     (llemacs--logging-write-error-pj (format "Code runution failed: %s" (error-message-string err)))
     nil)))
;; (llemacs--run-elisp '(message "hi"))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))