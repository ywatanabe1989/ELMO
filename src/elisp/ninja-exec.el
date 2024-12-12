;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:50:23
;;; Time-stamp: <2024-12-06 05:50:23 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-exec.el


(require 'ninja-config)
(require 'ninja-sudo)


(defun ninja-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (shell-quote-argument
       (condition-case err2
           (prin1-to-string code)
         (error
          (ninja--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
          nil)))
    (error
     (ninja--log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

;; python error is not captured
(defun ninja-exec-escaped-elisp-code (escaped-elisp-code)
  "Execute elisp code in the NINJA server process."
  (interactive)
  (condition-case err
      (progn
        (condition-case err2
            (ninja-init-or-connect)
          (error
           (ninja--log-error (format "Server connection failed: %s" (error-message-string err2)))
           (signal 'ninja-error (list "Server connection failed"))))
        (let ((cmd (condition-case err3
                      (format "echo %s | sudo -S %s execute %s"
                             (shell-quote-argument (ninja--sudo-get-password))
                             (shell-quote-argument ninja-server-script-path)
                             escaped-elisp-code)
                    (error
                     (ninja--log-error (format "Command formatting failed: %s" (error-message-string err3)))
                     (signal 'ninja-error (list "Command formatting failed"))))))
          (shell-command cmd)))
    (error
     (ninja--log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))

(defun ninja-exec-elisp-code (elisp-code)
  "Execute ELISP-CODE in NINJA server process."
  (interactive "xLisp expression: ")
  (condition-case err
      (let* ((code (if (stringp elisp-code)
                       (condition-case err2
                           (read elisp-code)
                         (error
                          (ninja--log-error (format "Code parsing failed: %s" (error-message-string err2)))
                          nil))
                     elisp-code))
             (escaped-elisp-code (when code (ninja-escape-elisp-code code))))
        (when (and code escaped-elisp-code)
          (ninja-exec-escaped-elisp-code escaped-elisp-code)))
    (error
     (ninja--log-error (format "Code execution preparation failed: %s" (error-message-string err)))
     nil)))


;; ;; Working
;; (ninja-exec-elisp-code '(load-file "/home/ninja/.emacs.d/init.el"))
;; (ninja-exec-elisp-code '(message "hi"))
;; (ninja-exec-elisp-code '(message "hello!!!!!"))


(provide 'ninja-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
