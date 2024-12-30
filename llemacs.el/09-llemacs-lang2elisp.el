;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:11:26
;;; Time-stamp: <2024-12-29 08:11:26 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/09-llemacs-lang2elisp.el

(require 'request)
(require '01-llemacs-config)
(require '04-llemacs-utils)
(require '02-llemacs-logging)
(require '08-llemacs-prompt)
(require '03-llemacs-llm)

;; (llemacs-llm "hello" "001-context-to-report")
;; (llemacs-lang2elisp "hello" "001-context-to-report")

;; "001-context-to-report"
;; (defun llemacs-prompt-to-elisp-including-response (prompt prompt-template-name)
;;   (condition-case err
;;       (llemacs-llm (llemacs-prompt-combine prompt prompt-template-name))
;;     (error
;;      (llemacs--log-prompt prompt)
;;      (llemacs--log-error (format "API request failed.\n%s"
;;                              (error-message-string err)))
;;      nil)))

(defun llemacs-extract-elisp-blocks (text)
  "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
  (let ((blocks nil)
        (start 0))
    (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (push (string-trim (match-string 1 text)) blocks)
      (setq start (match-end 0)))
    (if blocks
        (nreverse blocks)
      (error "No ELISP blocks found in response"))))

(defun llemacs-lang2elisp (prompt &optional template)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs-llm prompt template))
          (unless elisp-including-response
            (signal 'llemacs-api-error "No response received from API")))
      (error
       (llemacs--log-prompt prompt)
       (llemacs--log-error
        (format "API request failed.\n%s"
                (error-message-string err)))
       (signal 'llemacs-api-error err)))
    (when elisp-including-response
      (condition-case err
          (progn
            (setq elisp-blocks (llemacs-extract-elisp-blocks elisp-including-response))
            (unless elisp-blocks
              (signal 'llemacs-elisp-cleanup-error "No elisp blocks found in response")))
        (error
         (llemacs--log-error
          (format "Elisp extraction failed.\n%s\n%s"
                  (error-message-string err) elisp-including-response))
         (signal 'llemacs-elisp-cleanup-error err)))
      (condition-case err
          (progn
            (setq commands
                  (mapcar (lambda (block)
                            (read (concat "(progn " block ")")))
                          elisp-blocks))
            (unless commands
              (signal 'llemacs-elisp-parse-error "No valid elisp code generated")))
        (error
         (llemacs--log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'llemacs-elisp-parse-error err))))
    (if commands
        (cons 'progn commands)
      (signal 'llemacs-elisp-parse-error "No valid elisp code generated"))))

(provide '09-llemacs-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))