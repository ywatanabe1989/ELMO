;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 03:28:52
;;; Time-stamp: <2025-01-02 03:28:52 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/04-llemacs-cvt/lang2elisp.el

;; ----------------------------------------
;; Converters
;; ----------------------------------------
(defun llemacs--cvt-prompt2elisp (prompt &optional recipe-id)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs--run-prompt prompt recipe-id))
          (unless elisp-including-response
            (signal 'llemacs-api-error "No response received from API")))
      (error
       (llemacs--logging-log-prompt prompt)
       (llemacs--logging-log-error
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
         (llemacs--logging-log-error
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
         (llemacs--logging-log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'llemacs-elisp-parse-error err))))
    (if commands
        (cons 'progn commands)
      (signal 'llemacs-elisp-parse-error "No valid elisp code generated"))))

;; ----------------------------------------
;; Helpers
;; ----------------------------------------
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

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))