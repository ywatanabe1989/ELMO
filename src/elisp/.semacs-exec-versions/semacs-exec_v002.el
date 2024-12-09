;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 03:31:29
;;; Time-stamp: <2024-12-06 03:31:29 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-exec.el


(require 'semacs-config)
(require 'semacs-sudo)


;; (defun semacs-escape-elisp-code (code)
;;   "Escape elisp CODE for shell execution."
;;   (condition-case err
;;       (shell-quote-argument
;;        (condition-case err2
;;            (prin1-to-string code)
;;          (error
;;           (semacs--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
;;           nil)))
;;     (error
;;      (semacs--log-error (format "Code escaping failed: %s" (error-message-string err)))
;;      nil)))

(defun semacs-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (shell-quote-argument
       (condition-case err2
           (let ((code-str (prin1-to-string code)))
             (semacs--log-command code-str)
             code-str)
         (error
          (semacs--log-error (format "Code string conversion failed: %s" (error-message-string err2)))
          nil)))
    (error
     (semacs--log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

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
        (let ((escaped-elisp-code (if (stringp escaped-elisp-code)
                               escaped-elisp-code
                             (prin1-to-string (format "execute %s" escaped-elisp-code)))))
          (semacs--run-sudo-server-script escaped-elisp-code)))
    (error
     (semacs--log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))

(defun semacs-exec-elisp-code (elisp-code)
  "Execute ELISP-CODE in SEMACS server process."
  (interactive "xLisp expression: ")
  (condition-case err
      (let ((code (if (stringp elisp-code)
                      (condition-case err2
                          (read elisp-code)
                        (error
                         (semacs--log-error (format "Code parsing failed: %s" (error-message-string err2)))
                         nil))
                    elisp-code)))
        (when code
          (semacs-exec-escaped-elisp-code (semacs-escape-elisp-code code))))
    (error
     (semacs--log-error (format "Code execution preparation failed: %s" (error-message-string err)))
     nil)))


;; ;; Not Working
;; (semacs-exec-elisp-code '(message "hi"))
;; (semacs-exec-elisp-code '(message "hello!!!!!"))
;; (semacs-exec-elisp-code '(progn (message "Hello! I'm your self-evolving agent. How can I help you today?")))

(provide 'semacs-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
