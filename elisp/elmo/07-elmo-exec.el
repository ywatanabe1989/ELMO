;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 20:08:45
;;; Time-stamp: <2024-12-27 20:08:45 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/07-elmo-exec.el

(require '01-elmo-config)

(defun elmo-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (prin1-to-string code)
    (error
     (elmo-log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

(defun elmo-exec-local (escaped-elisp-code)
  "Execute elisp code in local Emacs."
  (interactive)
  (condition-case err
      (eval escaped-elisp-code)
    (error
     (elmo-log-error (format "Local code execution failed: %s" (error-message-string err)))
     nil)))

;; ;; Working
;; (elmo-exec-local '(message "hi"))
;; (elmo-exec-local '(message "hello!!!!!"))
;; (elmo-exec-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))

(defun elmo-exec-server (escaped-elisp-code &optional emacs-server-file)
  "Execute elisp code in remote Emacs server specified by EMACS-SERVER-FILE.
ESCAPED-ELISP-CODE is the elisp code to execute."
  (interactive)
  (condition-case err
      (let* ((server-file (or emacs-server-file elmo-server))
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
     (elmo-log-error (format "Server code execution failed: %s" (error-message-string err)))
     nil)))

;; ;; Working
;; Example usage:
;; (elmo-exec-server '(+ 1 2))

(defun elmo-exec-server-or-local (escaped-elisp-code)
  "Execute elisp code, trying server first then falling back to local."
  (interactive)
  (condition-case err
      (let* ((server-file (or elmo-server nil)))
        (if (and server-file
                 (file-exists-p server-file)
                 (file-readable-p server-file))
            ;; Try server first
            (elmo-exec-server escaped-elisp-code server-file)
          ;; Fall back to local if server unavailable
          (elmo-exec-local escaped-elisp-code)))
    (error
     (elmo-log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))

;; ;; Working
;; (elmo-exec-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))
;; (elmo-exec-local '(message "hi"))
;; (elmo-exec-local '(message "hello!!!!!"))

(provide '07-elmo-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))