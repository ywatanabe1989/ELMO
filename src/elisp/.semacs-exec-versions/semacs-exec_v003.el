;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:50:23
;;; Time-stamp: <2024-12-06 05:50:23 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-exec.el


(require 'semacs-config)
(require 'semacs-sudo)


(defun semacs-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (shell-quote-argument
       (condition-case err2
           (prin1-to-string code)
         (error
          (semacs--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
          nil)))
    (error
     (semacs--log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

;; python error is not captured
(defun semacs-exec-escaped-elisp-code (escaped-elisp-code)
  "Execute elisp code in the SEMACS server process."
  (interactive)
  (condition-case err
      (progn
        (condition-case err2
            (semacs-init-or-connect)
          (error
           (semacs--log-error (format "Server connection failed: %s" (error-message-string err2)))
           (signal 'semacs-error (list "Server connection failed"))))
        (let ((cmd (condition-case err3
                      (format "echo %s | sudo -S %s execute %s"
                             (shell-quote-argument (semacs--sudo-get-password))
                             (shell-quote-argument semacs-server-script-path)
                             escaped-elisp-code)
                    (error
                     (semacs--log-error (format "Command formatting failed: %s" (error-message-string err3)))
                     (signal 'semacs-error (list "Command formatting failed"))))))
          (shell-command cmd)))
    (error
     (semacs--log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))

(defun semacs-exec-elisp-code (elisp-code)
  "Execute ELISP-CODE in SEMACS server process."
  (interactive "xLisp expression: ")
  (condition-case err
      (let* ((code (if (stringp elisp-code)
                       (condition-case err2
                           (read elisp-code)
                         (error
                          (semacs--log-error (format "Code parsing failed: %s" (error-message-string err2)))
                          nil))
                     elisp-code))
             (escaped-elisp-code (when code (semacs-escape-elisp-code code))))
        (when (and code escaped-elisp-code)
          (semacs-exec-escaped-elisp-code escaped-elisp-code)))
    (error
     (semacs--log-error (format "Code execution preparation failed: %s" (error-message-string err)))
     nil)))


;; ;; Working
;; (semacs-exec-elisp-code '(load-file "/home/semacs/.emacs.d/init.el"))
;; (semacs-exec-elisp-code '(message "hi"))
;; (semacs-exec-elisp-code '(message "hello!!!!!"))


(provide 'semacs-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
