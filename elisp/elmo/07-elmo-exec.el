;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-25 00:49:03
;;; Time-stamp: <2024-12-25 00:49:03 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/07-elmo-exec.el

(require '01-elmo-config)

(defun elmo-escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (prin1-to-string code)
    (error
     (elmo-log-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))

(defun elmo-exec-escaped-elisp-code-local (escaped-elisp-code)
  "Execute elisp code in local Emacs."
  (interactive)
  (condition-case err
      (eval escaped-elisp-code)
    (error
     (elmo-log-error (format "Local code execution failed: %s" (error-message-string err)))
     nil)))

;; ;; Working
;; (elmo-exec-escaped-elisp-code-local '(message "hi"))
;; (elmo-exec-escaped-elisp-code-local '(message "hello!!!!!"))
;; (elmo-exec-escaped-elisp-code-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))

(defun elmo-exec-escaped-elisp-code-server (escaped-elisp-code &optional emacs-server-file)
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
;; (elmo-exec-escaped-elisp-code-server '(+ 1 2))

;; ;; python error is not captured
;; (defun elmo-exec-escaped-elisp-code (escaped-elisp-code)
;;   "Execute elisp code in the ELMO server process."
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (condition-case err2
;;             (elmo-init-or-connect)
;;           (error
;;            (elmo-log-error (format "Server connection failed: %s" (error-message-string err2)))
;;            (signal 'elmo-error (list "Server connection failed"))))
;;         (let ((cmd (condition-case err3
;;                       (format "echo %s | sudo -S %s execute %s"
;;                              (shell-quote-argument (elmo-sudo-get-password))
;;                              (shell-quote-argument elmo-server-script-path)
;;                              escaped-elisp-code)
;;                     (error
;;                      (elmo-log-error (format "Command formatting failed: %s" (error-message-string err3)))
;;                      (signal 'elmo-error (list "Command formatting failed"))))))
;;           (shell-command cmd)))
;;     (error
;;      (elmo-log-error (format "Code execution failed: %s" (error-message-string err)))
;;      nil)))

;; (defun elmo-exec-elisp-code (elisp-code)
;;   "Execute ELISP-CODE in ELMO server process."
;;   (interactive "xLisp expression: ")
;;   (condition-case err
;;       (let* ((code (if (stringp elisp-code)
;;                        (condition-case err2
;;                            (read elisp-code)
;;                          (error
;;                           (elmo-log-error (format "Code parsing failed: %s" (error-message-string err2)))
;;                           nil))
;;                      elisp-code))
;;              (escaped-elisp-code (when code (elmo-escape-elisp-code code))))
;;         (when (and code escaped-elisp-code)
;;           (elmo-exec-escaped-elisp-code escaped-elisp-code)))
;;     (error
;;      (elmo-log-error (format "Code execution preparation failed: %s" (error-message-string err)))
;;      nil)))


(defun elmo-exec-escaped-elisp-code (escaped-elisp-code)
  "Execute elisp code, trying server first then falling back to local."
  (interactive)
  (condition-case err
      (let* ((server-file (or elmo-server nil)))
        (if (and server-file
                 (file-exists-p server-file)
                 (file-readable-p server-file))
            ;; Try server first
            (elmo-exec-escaped-elisp-code-server escaped-elisp-code server-file)
          ;; Fall back to local if server unavailable
          (elmo-exec-escaped-elisp-code-local escaped-elisp-code)))
    (error
     (elmo-log-error (format "Code execution failed: %s" (error-message-string err)))
     nil)))



;; (defun elmo-exec-elisp-code (code)
;;   "Execute elisp CODE safely with proper error handling."
;;   (interactive "xLisp expression: ")
;;   (condition-case err
;;       (let ((parsed-code (cond
;;                           ((stringp code)
;;                            (condition-case err
;;                                (read code)
;;                              (error
;;                               (elmo-log-error (format "Failed to parse string code: %s" err))
;;                               nil)))
;;                           ((listp code) code)
;;                           (t (error "Invalid code type: %s" (type-of code))))))
;;         (when parsed-code
;;           (condition-case err
;;               (elmo-exec-escaped-elisp-code parsed-code)
;;             (error
;;              (elmo-log-error (format "Failed to execute code: %s" err))
;;              nil))))
;;     (error
;;      (elmo-log-error (format "Top-level execution error: %s" err))
;;      nil)))


;; ;; Working
;; (elmo-exec-escaped-elisp-code-local '(load-file "/home/ywatanabe/.emacs.d/init.el"))
;; (elmo-exec-escaped-elisp-code-local '(message "hi"))
;; (elmo-exec-escaped-elisp-code-local '(message "hello!!!!!"))


(defalias 'elmo-exec 'elmo-exec-escaped-elisp-code-local)
(defalias 'elmo-exec-server 'elmo-exec-escaped-elisp-code-server)

(provide '07-elmo-exec)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))