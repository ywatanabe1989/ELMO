;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:23:03
;;; Time-stamp: <2024-12-31 23:23:03 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/05-llemacs-run/05-llemacs-exec.el

(require '01-llemacs-config)

(defun llemacs-exec-local (escaped-elisp-code)
  "Execute elisp code in local Emacs."
  (interactive)
  (condition-case err
      (eval escaped-elisp-code)
    (error
     (llemacs--logging-error (format "Local code execution failed: %s" (error-message-string err)))
     nil)))

(defun llemacs-exec-server (escaped-elisp-code &optional emacs-server-file)
  "Execute elisp code in remote Emacs server specified by EMACS-SERVER-FILE.
ESCAPED-ELISP-CODE is the elisp code to execute."
  (interactive)
  (condition-case err
      (let* ((server-file (or emacs-server-file llemacs-path-emacs-server))
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
     (llemacs--logging-error (format "Server code execution failed: %s" (error-message-string err)))
     nil)))

(defun llemacs-exec-server-or-local (escaped-elisp-code)
  "Execute elisp code, trying server first then falling back to local."
  (interactive)
  (condition-case err
      (let* ((server-file (or llemacs-path-emacs-server nil)))
        (if (and server-file
                 (file-exists-p server-file)
                 (file-readable-p server-file))
            ;; Try server first
            (llemacs-exec-server escaped-elisp-code server-file)
          ;; Fall back to local if server unavailable
          (llemacs-exec-local escaped-elisp-code)))
    (error
     (llemacs--logging-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))



;; ;; Working
;; (llemacs-exec-local '(message "hi"))
;; (llemacs-exec-local '(message "hello!!!!!"))
;; (llemacs-exec-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))

;; ;; Working
;; Example usage:
;; (llemacs-exec-server '(+ 1 2))

;; ;; Working
;; (llemacs-exec-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))
;; (llemacs-exec-local '(message "hi"))
;; (llemacs-exec-local '(message "hello!!!!!"))

(provide '04-llemacs-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))