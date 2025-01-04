;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 23:14:08
;;; Time-stamp: <2025-01-04 23:14:08 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/lang2elisp.el

;; ----------------------------------------
;; Converters
;; ----------------------------------------
(defvar elisp-including-response "" "")


;; (defun llemacs--cvt-prompt2elisp (prompt &optional recipe-id)
;;   (let ((elisp-including-response nil)
;;         (elisp-blocks nil)
;;         (commands nil))
;;     (condition-case err
;;         (progn
;;           (setq elisp-including-response (llemacs-llm prompt recipe-id))
;;           (unless elisp-including-response
;;             (signal 'llemacs-api-error "No response received from API")))
;;       (error
;;        (llemacs--logging-write-prompt-pj prompt)
;;        (llemacs--logging-write-error-pj
;;         (format "API request failed.\n%s"
;;                 (error-message-string err)))
;;        (signal 'llemacs-api-error err)))
;;     (when elisp-including-response
;;       (condition-case err
;;           (progn
;;             (setq elisp-blocks (llemacs-extract-elisp-blocks elisp-including-response))
;;             (unless elisp-blocks
;;               (signal 'llemacs-elisp-cleanup-error "No elisp blocks found in response")))
;;         (error
;;          (llemacs--logging-write-error-pj
;;           (format "Elisp extraction failed.\n%s\n%s"
;;                   (error-message-string err) elisp-including-response))
;;          (signal 'llemacs-elisp-cleanup-error err)))
;;       (condition-case err
;;           (progn
;;             (setq commands
;;                   (mapcar (lambda (block)
;;                             (read (concat "(progn " block ")")))
;;                           elisp-blocks))
;;             (unless commands
;;               (signal 'llemacs-elisp-parse-error "No valid elisp code generated")))
;;         (error
;;          (llemacs--logging-write-error-pj
;;           (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
;;                   (error-message-string err) elisp-blocks))
;;          (signal 'llemacs-elisp-parse-error err))))
;;     (if commands
;;         (cons 'progn commands)
;;       (signal 'llemacs-elisp-parse-error "No valid elisp code generated"))))

(defun llemacs--cvt-prompt2elisp (prompt &optional recipe-id)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (message "Debug: Starting conversion with prompt: %s" prompt)
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs-llm prompt recipe-id))
          (message "Debug: Raw LLM response: %s" elisp-including-response)
          (unless elisp-including-response
            (signal 'llemacs-api-error "No response received from API")))
      (error
       (message "Debug: API error occurred: %s" (error-message-string err))
       (llemacs--logging-write-prompt-pj prompt)
       (llemacs--logging-write-error-pj
        (format "API request failed.\n%s"
                (error-message-string err)))
       (signal 'llemacs-api-error err)))
    (when elisp-including-response
      (message "Debug: Processing elisp blocks")
      (condition-case err
          (progn
            (setq elisp-blocks (llemacs-extract-elisp-blocks elisp-including-response))
            (message "Debug: Extracted blocks: %S" elisp-blocks)
            (unless elisp-blocks
              (signal 'llemacs-elisp-cleanup-error "No elisp blocks found in response")))
        (error
         (message "Debug: Block extraction error: %s" (error-message-string err))
         (llemacs--logging-write-error-pj
          (format "Elisp extraction failed.\n%s\n%s"
                  (error-message-string err) elisp-including-response))
         (signal 'llemacs-elisp-cleanup-error err)))
      (condition-case err
          (progn
            (setq commands
                  (mapcar (lambda (block)
                            (message "Debug: Parsing block: %s" block)
                            (read (concat "(progn " block ")")))
                          elisp-blocks))
            (message "Debug: Generated commands: %S" commands)
            (unless commands
              (signal 'llemacs-elisp-parse-error "No valid elisp code generated")))
        (error
         (message "Debug: Parse error: %s" (error-message-string err))
         (llemacs--logging-write-error-pj
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'llemacs-elisp-parse-error err))))
    (if commands
        (cons 'progn commands)
      (signal 'llemacs-elisp-parse-error "No valid elisp code generated"))))

;; (llemacs--cvt-prompt2elisp "hello" "code-gen")
;; if: Format specifier doesn’t match argument type

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