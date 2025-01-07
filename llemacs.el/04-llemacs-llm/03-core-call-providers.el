;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 05:26:36
;;; Timestamp: <2025-01-08 05:26:36>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/03-core-call-providers.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'json)
(require 'request)
(require 'cl)

(defcustom llemacs-llm-provider "anthropic"
  "Switcher for LLM provider"
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-gemini-script
  (expand-file-name "03-core-gemini_call.py" (file-name-directory (or load-file-name buffer-file-name)))
  "Python script for calling Gemini"
  :type 'string
  :group 'llemacs-llm)

(defun llemacs--llm-switch-provider (provider)
  "Switch the LLM provider."
  (interactive
   (list (completing-read "Select LLM provider: "
                          '("deepseek" "openai" "anthropic" "google")
                          nil t)))
  (setq llemacs-llm-provider provider)
  (message "Switched LLM provider to: %s" provider))

;; Gemini
(defun llemacs--llm-gemini (text)
  "Simple text to text processing using Gemini API."
  (condition-case err
      (let ((temp-file (make-temp-file "gemini-response")))
        (unwind-protect
            (progn
              (unless (= 0 (shell-command
                            (format "%s %s %s %s"
                                    llemacs--path-python-sys
                                    llemacs--llm-gemini-script
                                    (shell-quote-argument text)
                                    (shell-quote-argument temp-file))))
                (error
                 (llemacs--logging-write-error-sys "Python script execution failed")))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (buffer-string)))
          (ignore-errors (delete-file temp-file))))
    (error
     (llemacs--logging-write-error-sys (format "Gemini API request failed.\n%s"
                                               (error-message-string err)))
     nil)))

;; Claude
(defun llemacs--llm-claude (text)
  "Simple text to text processing using Claude API."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("x-api-key" . ,llemacs--llm-anthropic-key)
                ("anthropic-version" . "2023-06-01")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("model" . ,llemacs--llm-anthropic-engine)
                  ("max_tokens" . 8192)
                  ("messages" . [,(list (cons "role" "user")
                                        (cons "content" text))])))
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
                        (let ((resp-data (json-read)))
                          (when resp-data
                            (alist-get 'text (aref (alist-get 'content resp-data) 0))))))
                  (error "API request failed with status: %s" status-line))))
          (error "Failed to retrieve response")))
    (error
     (llemacs--logging-write-error-sys (format "Claude API request failed.\n%s"
                                               (error-message-string err)))
     nil)))

;; DeepSeek
(defun llemacs--llm-deepseek (text)
  "Simple text to text processing using DeepSeek API."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " llemacs--llm-deepseek-key))))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("model" . ,llemacs--llm-deepseek-engine)
                  ("messages" . [,(list (cons "role" "system")
                                        (cons "content" "You are a helpful assistant."))
                                 ,(list (cons "role" "user")
                                        (cons "content" text))])
                  ("stream" . :json-false)))
               'utf-8))
             (buffer (url-retrieve-synchronously
                      "https://api.deepseek.com/chat/completions" t)))
        (if buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (let ((status-line (buffer-substring (point) (line-end-position))))
                (if (string-match "200 OK" status-line)
                    (progn
                      (re-search-forward "^$")
                      (let ((json-object-type 'alist)
                            (json-array-type 'vector))
                        (let ((resp-data (json-read)))
                          (when resp-data
                            (let* ((choices (alist-get 'choices resp-data))
                                   (first-choice (aref choices 0))
                                   (message (alist-get 'message first-choice))
                                   (content (alist-get 'content message)))
                              (if (stringp content)
                                  (decode-coding-string content 'utf-8)
                                (error "Invalid content in API response")))))))
                  (error "DeepSeek API request failed with status: %s" status-line))))
          (error "Failed to retrieve response from DeepSeek API")))
    (error
     (llemacs--logging-write-error-sys (format "DeepSeek API request failed.\n%s"
                                               (error-message-string err)))
     nil)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
