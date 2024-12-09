;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 03:31:29
;;; Time-stamp: <2024-12-06 03:31:29 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-exec.el


(require 'ninja-config)
(require 'ninja-sudo)


;; (defun ninja-escape-elisp-code (code)
;;   "Escape elisp CODE for shell execution."
;;   (condition-case err
;;       (shell-quote-argument
;;        (condition-case err2
;;            (prin1-to-string code)
;;          (error
;;           (ninja--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
;;           nil)))
;;     (error
;;      (ninja--log-error (format "Code escaping failed: %s" (error-message-string err)))
;;      nil)))

(defun ninja-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (shell-quote-argument
       (condition-case err2
           (let ((code-str (prin1-to-string code)))
             (ninja--log-command code-str)
             code-str)
         (error
          (ninja--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
          nil)))
    (error
     (ninja--log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

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
        (let ((escaped-elisp-code (if (stringp escaped-elisp-code)
                               escaped-elisp-code
                             (prin1-to-string (format "execute %s" escaped-elisp-code)))))
          (ninja--run-sudo-server-script escaped-elisp-code)))
    (error
     (ninja--log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))

(defun ninja-exec-elisp-code (elisp-code)
  "Execute ELISP-CODE in NINJA server process."
  (interactive "xLisp expression: ")
  (condition-case err
      (let ((code (if (stringp elisp-code)
                      (condition-case err2
                          (read elisp-code)
                        (error
                         (ninja--log-error (format "Code parsing failed: %s" (error-message-string err2)))
                         nil))
                    elisp-code)))
        (when code
          (ninja-exec-escaped-elisp-code (ninja-escape-elisp-code code))))
    (error
     (ninja--log-error (format "Code execution preparation failed: %s" (error-message-string err)))
     nil)))


;; ;; Not Working
;; (ninja-exec-elisp-code '(message "hi"))
;; (ninja-exec-elisp-code '(message "hello!!!!!"))
;; (ninja-exec-elisp-code '(progn (message "Hello! I'm your self-evolving agent. How can I help you today?")))

(provide 'ninja-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
