;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 22:35:45
;;; Timestamp: <2025-01-08 22:35:45>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/08-prompt-to-elisp.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defvar elisp-including-response "" "")

(defun llemacs--llm-prompt2elisp (prompt &optional recipe-id)
  "Convert PROMPT to Elisp code using LLM."
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs-llm prompt recipe-id))
          (unless elisp-including-response
            (error "No response received from API"))
          (llemacs--logging-write-elisp-pj elisp-including-response)
          (setq elisp-blocks (llemacs-extract-elisp-blocks elisp-including-response))
          (unless elisp-blocks
            (error "No elisp blocks found in response"))
          (setq commands
                (mapcar (lambda (block)
                          (read (concat "(progn " block ")")))
                        elisp-blocks))
          (unless commands
            (error "No valid elisp code generated"))
          (llemacs--logging-write-info-pj commands)
          (cons 'progn commands))
      (error
       (llemacs--logging-write-error-pj
        (format "Conversion failed: %s" (error-message-string err)))
       nil))))

;; Helper
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
