;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 11:42:17
;;; Time-stamp: <2024-12-31 11:42:17 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm.el

(require 'request)
(require 'json)
(require '01-llemacs-config)
(require '02-llemacs-logging)
(require '04-llemacs-utils)
(require '08-llemacs-prompt)

(defvar llemacs-llm-provider "deepseek"
  "Switcher for LLM provider")

(defcustom llemacs-gemini-script (expand-file-name "gemini_call.py" llemacs-path-scripts-python)
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

(defvar llemacs-deepseek-key (getenv "DEEPSEEK_API_KEY")
  "API key for DeepSeek.")

(defvar llemacs-deepseek-engine (getenv "DEEPSEEK_ENGINE")
  "Model for DeepSeek.")

(defun llemacs-llm-switch-provider (provider)
  "Switch the LLM provider."
  (interactive
   (list (completing-read "Select LLM provider: "
                          '("deepseek" "openai" "anthropic" "google")
                          nil t)))
  (setq llemacs-llm-provider provider)
  (message "Switched LLM provider to: %s" provider))

(defun llemacs-llm-gemini (text)
  "Simple text to text processing using Gemini API."
  (condition-case err
      (let ((temp-file (make-temp-file "gemini-response")))
        (unwind-protect
            (progn
              (unless (= 0 (shell-command
                            (format "source /workspace/.env/bin/activate && python3 %s \"%s\" \"%s\""
                                    llemacs-gemini-script
                                    (replace-regexp-in-string "\"" "\\\\\"" text)
                                    temp-file)))
                (error "Python script execution failed"))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (buffer-string)))
          (ignore-errors (delete-file temp-file))))
    (error
     (llemacs--log-error (format "Gemini API request failed.\n%s"
                              (error-message-string err)))
     nil)))

(defun llemacs-llm-claude (text)
  "Simple text to text processing using Claude API."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("x-api-key" . ,llemacs-anthropic-key)
                ("anthropic-version" . "2023-06-01")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("model" . ,llemacs-anthropic-engine)
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
     (llemacs--log-error (format "Claude API request failed.\n%s"
                              (error-message-string err)))
     nil)))


(defun llemacs-llm-deepseek (text)
  "Simple text to text processing using DeepSeek API."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " llemacs-deepseek-key))))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("model" . ,llemacs-deepseek-engine)
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
                  (error "API request failed with status: %s" status-line))))
          (error "Failed to retrieve response")))
    (error
     (llemacs--log-error (format "DeepSeek API request failed.\n%s"
                              (error-message-string err)))
     nil)))


(defun llemacs-llm (prompt &optional template)
  "Process PROMPT using configured LLM provider.
Optional TEMPLATE is used to combine with prompt."
  (let ((text (if template
                  (llemacs-to-full-prompt template prompt)
                prompt)))
    (pcase llemacs-llm-provider
      ("anthropic" (llemacs-llm-claude text))
      ("google" (llemacs-llm-gemini text))
      ("deepseek" (llemacs-llm-deepseek text))
      (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))


;; (llemacs-llm "hello")
;; (llemacs-llm-deepseek "hello")
;; (llemacs-llm "hello" "001-context-to-report")

(provide '03-llemacs-llm)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))