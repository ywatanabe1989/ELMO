;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 19:36:26
;;; Time-stamp: <2024-12-27 19:36:26 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/09-llemacs-lang2elisp.el

(require 'request)
(require '01-llemacs-config)
(require '04-llemacs-utils)
(require '02-llemacs-logging-core)
(require '08-llemacs-prompt-templates)

(defcustom llemacs-pyscripts-dir (expand-file-name "resources/scripts/" llemacs-workspace-dir)
  "Path to the Python binary used by llemacs.el."
  :type 'string)

(defcustom llemacs-gemini-script (expand-file-name "gemini_call.py" llemacs-pyscripts-dir)
  "Path to the Python binary used by llemacs.el."
  :type 'string)

(defvar llemacs-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar llemacs-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude.")

(defvar llemacs-google-key (getenv "GOOGLE_API_KEY")
  "API key for Google Claude.")

(defvar llemacs-google-engine (getenv "GOOGLE_ENGINE")
  "Model for Google Claude.")

(defvar llemacs-llm-provider "anthropic"
  "Switcher for LLM provider")

(defun llemacs-prompt-to-elisp-including-response (prompt)
  (pcase llemacs-llm-provider
    ("anthropic" (llemacs-prompt-to-elisp-including-response-claude prompt))
    ("google" (llemacs-prompt-to-elisp-including-response-gemini prompt))
    (_ (error "Unknown LLM provider: %s" llemacs-llm-provider))))

;; (llemacs-prompt-to-elisp-including-response "hello")

(defun llemacs-prompt-to-elisp-including-response-gemini (prompt)
  (condition-case err
      (let* ((full-prompt (llemacs-to-full-prompt "001-context-to-report" prompt))
             (temp-file (make-temp-file "gemini-response")))
        (unwind-protect
            (progn
              (unless (= 0 (shell-command
                            (format "source /workspace/.env/bin/activate && python3 %s \"%s\" \"%s\""
                                    llemacs-gemini-script
                                    (replace-regexp-in-string "\"" "\\\\\"" full-prompt)
                                    temp-file)))
                (error "Python script execution failed"))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (buffer-string)))
          (ignore-errors
            (delete-file temp-file))))
    (error
     (llemacs-log-prompt prompt)
     (llemacs-log-error (format "Gemini API request failed.\n%s"
                             (error-message-string err)))
     nil)))

;; (llemacs-prompt-to-elisp-including-response-gemini "hello")

(defun llemacs-prompt-to-elisp-including-response-claude (prompt)
  (condition-case err
      (let* ((full-prompt (llemacs-to-full-prompt "001-context-to-report" prompt))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("x-api-key" . ,llemacs-anthropic-key)
                ("anthropic-version" . "2023-06-01")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("model" . ,llemacs-anthropic-engine)
                  ("max_tokens" . 8192)
                  ("system" . "Return only raw Elisp code without any markup or comments.")
                  ("messages" . [,(list (cons "role" "user")
                                        (cons "content" full-prompt))])))
               'utf-8))
             (buffer (url-retrieve-synchronously
                      "https://api.anthropic.com/v1/messages" t)))

        (if buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (let ((status-line (buffer-substring (point) (line-end-position))))
                (if (string-match "200 OK" status-line)
                    (progn
                      (re-search-forward "^$")
                      (let ((json-object-type 'alist)
                            (json-array-type 'vector))
                        (condition-case nil
                            (let ((resp-data (json-read)))
                              (when resp-data
                                (alist-get 'text (aref (alist-get 'content resp-data) 0))))
                          (error nil))))
                  (error "API request failed with status: %s" status-line))))
          (error "Failed to retrieve response")))
    (error
     (llemacs-log-prompt prompt)
     (llemacs-log-error (format "API request failed.\n%s"
                             (error-message-string err)))
     nil)))

;; (llemacs-prompt-to-elisp-including-response-claude "hello")

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

(defun llemacs-lang2elisp (prompt)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (progn
          (setq elisp-including-response (llemacs-prompt-to-elisp-including-response prompt))
          (unless elisp-including-response
            (signal 'llemacs-api-error "No response received from API")))
      (error
       (llemacs-log-prompt prompt)
       (llemacs-log-error
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
         (llemacs-log-error
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
         (llemacs-log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'llemacs-elisp-parse-error err))))
    (if commands
        (cons 'progn commands)
      (signal 'llemacs-elisp-parse-error "No valid elisp code generated"))))

;; (llemacs-lang2elisp "hello")

(provide '09-llemacs-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))