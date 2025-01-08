;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 07:58:48
;;; Timestamp: <2025-01-09 07:58:48>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/08-prompt-to-elisp.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


(defvar elisp-including-response "" "Store the LLM response containing Elisp code.")


(defun llemacs--llm-prompt2elisp (prompt &optional recipe-id)
  "Convert PROMPT to Elisp code using LLM."
  (let ((elisp-including-response nil)
        (elisp-blocks nil))
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs-llm prompt recipe-id))
          ;; (message (format "elisp-including-response:\n%s" elisp-including-response))
          (unless elisp-including-response
            (error "No response received from API"))
          ;; (llemacs--logging-write-elisp-pj elisp-including-response)
          (setq elisp-blocks (llemacs-extract-elisp-blocks-as-code elisp-including-response))
          (unless elisp-blocks
            (error "No elisp blocks found in response"))
          ;; (llemacs--logging-write-info-pj elisp-blocks)
          (cons 'progn elisp-blocks))
      (error
       (error "Conversion failed: %s" (error-message-string err))))))

;; (defun llemacs--llm-prompt2elisp (prompt &optional recipe-id)
;;   "Convert PROMPT to Elisp code using LLM."
;;   (let ((elisp-including-response nil)
;;         (elisp-blocks nil)
;;         (commands nil))
;;     (condition-case err
;;         (progn
;;           (setq elisp-including-response (llemacs-llm prompt recipe-id))
;;           (unless elisp-including-response
;;             (error "No response received from API"))
;;           (llemacs--logging-write-elisp-pj elisp-including-response)
;;           (setq elisp-blocks (llemacs-extract-elisp-blocks-as-code elisp-including-response))
;;           (unless elisp-blocks
;;             (error "No elisp blocks found in response"))
;;           (setq commands
;;                 (mapcar (lambda (block)
;;                           (read (concat "(progn " block ")")))
;;                         elisp-blocks))
;;           (unless commands
;;             (error "No valid elisp code generated"))
;;           (llemacs--logging-write-info-pj commands)
;;           (cons 'progn commands))
;;       (error
;;        (error "Conversion failed: %s" (error-message-string err))))))

;; (defvar elisp-including-response "" "")

;; (defun llemacs--llm-prompt2elisp (prompt &optional recipe-id)
;;   "Convert PROMPT to Elisp code using LLM."
;;   (let ((elisp-including-response nil)
;;         (elisp-blocks nil)
;;         (commands nil))
;;     (condition-case err
;;         (progn
;;           (setq elisp-including-response (llemacs-llm prompt recipe-id))
;;           (unless elisp-including-response
;;             (error "No response received from API"))
;;           (llemacs--logging-write-elisp-pj elisp-including-response)
;;           (setq elisp-blocks (llemacs-extract-elisp-blocks elisp-including-response))
;;           (unless elisp-blocks
;;             (error "No elisp blocks found in response"))
;;           (setq commands
;;                 (mapcar (lambda (block)
;;                           (read (concat "(progn " block ")")))
;;                         elisp-blocks))
;;           (unless commands
;;             (error "No valid elisp code generated"))
;;           (llemacs--logging-write-info-pj commands)
;;           (cons 'progn commands))
;;       (error "Conversion failed: %s" (error-message-string err)))))

;; Helper
;; (defun llemacs-extract-elisp-blocks (text)
;;   "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
;;   (let ((blocks nil)
;;         (start 0))
;;     (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
;;       (push (string-trim (match-string 1 text)) blocks)
;;       (setq start (match-end 0)))
;;     (if blocks
;;         (nreverse blocks)
;;       (error "No ELISP blocks found in response"))))

(defun llemacs-extract-elisp-blocks (text)
  "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
  (let ((blocks nil)
        (start 0))
    (condition-case err
        (progn
          (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
            (let ((code (string-trim (match-string 1 text))))
              (unless (string-empty-p code)
                (push code blocks)))
            (setq start (match-end 0)))
          (if blocks
              (nreverse blocks)
            (error "No valid ELISP blocks found")))
      (error
       (error "Failed to extract ELISP blocks: %s" (error-message-string err))))))

;; ;; evaluate the string as code
;; (eval (read (car (llemacs-extract-elisp-blocks "aaaaaaaaaaaaaaaa ```elisp\n(message \"hello are you okay?\")\n```"))))
;; this returns: "(message \"hello are you okay?\")"


(defun llemacs-extract-elisp-blocks-as-code (text)
  "Extract ELISP blocks as code (not evaluated) from TEXT."
  (let ((blocks nil)
        (start 0))
    (condition-case err
        (progn
          (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
            (let ((code (string-trim (match-string 1 text))))
              (unless (string-empty-p code)
                (push (read code) blocks)))
            (setq start (match-end 0)))
          (if blocks
              (nreverse blocks)
            (error "No valid ELISP blocks found")))
      (error
       (error "Failed to extract ELISP blocks: %s" (error-message-string err))))))

;; Usage example - returns code without evaluating
;; (eval (cons 'progn (llemacs-extract-elisp-blocks-as-code "```elisp\n(message \"hello are you okay?\")\n```")))

;; (eval (llemacs--llm-prompt2elisp "plot something" "code-elisp-progn")) ;; working

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
