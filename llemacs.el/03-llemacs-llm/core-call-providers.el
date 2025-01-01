;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:29:01
;;; Time-stamp: <2024-12-31 22:29:01 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/03-llemacs-llm-call-providers.el

(require 'json)
(require 'request)

(defcustom llemacs-llm-provider "anthropic"
  "Switcher for LLM provider"
  :type 'string
  :group 'llemacs-llm)

(defun llemacs-llm-switch-provider (provider)
  "Switch the LLM provider."
  (interactive
   (list (completing-read "Select LLM provider: "
                          '("deepseek" "openai" "anthropic" "google")
                          nil t)))
  (setq llemacs-llm-provider provider)
  (message "Switched LLM provider to: %s" provider))

(defcustom llemacs-llm-gemini-script
  (expand-file-name "gemini_call.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to Python script for Gemini API calls."
  :type 'string
  :group 'llemacs-llm)

(defun llemacs--llm-gemini (text)
  "Simple text to text processing using Gemini API."
  (condition-case err
      (let ((temp-file (make-temp-file "gemini-response")))
        (unwind-protect
            (progn
              (unless (= 0 (shell-command
                            (format "source /workspace/.env/bin/activate && python3 %s \"%s\" \"%s\""
                                    llemacs--llm-gemini-script
                                    (replace-regexp-in-string "\"" "\\\\\"" text)
                                    temp-file)))
                (error "Python script execution failed"))
              (with-temp-buffer
                (insert-file-contents temp-file)
                (buffer-string)))
          (ignore-errors (delete-file temp-file))))
    (error
     (llemacs--logging-log-error (format "Gemini API request failed.\n%s"
                                     (error-message-string err)))
     nil)))

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
     (llemacs--logging-log-error (format "Claude API request failed.\n%s"
                                     (error-message-string err)))
     nil)))

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
                  (error "API request failed with status: %s" status-line))))
          (error "Failed to retrieve response")))
    (error
     (llemacs--logging-log-error (format "DeepSeek API request failed.\n%s"
                                     (error-message-string err)))
     nil)))

(provide '03-llemacs-llm-call-providers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))