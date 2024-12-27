;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 20:24:15
;;; Time-stamp: <2024-12-27 20:24:15 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/09-elmo-lang2elisp.el

(require 'request)
(require '01-elmo-config)
(require '04-elmo-utils)
(require '02-elmo-logging-core)
(require '08-elmo-prompt)
(require '09-elmo-llm)


(defun elmo-prompt-to-elisp-including-response (prompt)
  (condition-case err
      (elmo-llm (elmo-prompt-combine prompt "001-context-to-report"))
    (error
     (elmo-log-prompt prompt)
     (elmo-log-error (format "API request failed.\n%s"
                             (error-message-string err)))
     nil)))

(defun elmo-extract-elisp-blocks (text)
  "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
  (let ((blocks nil)
        (start 0))
    (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (push (string-trim (match-string 1 text)) blocks)
      (setq start (match-end 0)))
    (if blocks
        (nreverse blocks)
      (error "No ELISP blocks found in response"))))

(defun elmo-lang2elisp (prompt)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (progn
          (setq elisp-including-response (elmo-prompt-to-elisp-including-response prompt))
          (unless elisp-including-response
            (signal 'elmo-api-error "No response received from API")))
      (error
       (elmo-log-prompt prompt)
       (elmo-log-error
        (format "API request failed.\n%s"
                (error-message-string err)))
       (signal 'elmo-api-error err)))
    (when elisp-including-response
      (condition-case err
          (progn
            (setq elisp-blocks (elmo-extract-elisp-blocks elisp-including-response))
            (unless elisp-blocks
              (signal 'elmo-elisp-cleanup-error "No elisp blocks found in response")))
        (error
         (elmo-log-error
          (format "Elisp extraction failed.\n%s\n%s"
                  (error-message-string err) elisp-including-response))
         (signal 'elmo-elisp-cleanup-error err)))
      (condition-case err
          (progn
            (setq commands
                  (mapcar (lambda (block)
                            (read (concat "(progn " block ")")))
                          elisp-blocks))
            (unless commands
              (signal 'elmo-elisp-parse-error "No valid elisp code generated")))
        (error
         (elmo-log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'elmo-elisp-parse-error err))))
    (if commands
        (cons 'progn commands)
      (signal 'elmo-elisp-parse-error "No valid elisp code generated"))))

(provide '09-elmo-lang2elisp)
(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (defun elmo-prompt-to-elisp-including-response (prompt)
;;   (pcase elmo-llm-provider
;;     ("anthropic" (elmo-prompt-to-elisp-including-response-claude prompt))
;;     ("google" (elmo-prompt-to-elisp-including-response-gemini prompt))
;;     (_ (error "Unknown LLM provider: %s" elmo-llm-provider))))

;; ;; (elmo-prompt-to-elisp-including-response "hello")

;; (defun elmo-prompt-to-elisp-including-response-gemini (prompt)
;;   (condition-case err
;;       (let* ((full-prompt (elmo-prompt-combine "001-context-to-report" prompt))
;;              (temp-file (make-temp-file "gemini-response")))
;;         (unwind-protect
;;             (progn
;;               (unless (= 0 (shell-command
;;                             (format "source /workspace/.env/bin/activate && python3 %s \"%s\" \"%s\""
;;                                     elmo-gemini-script
;;                                     (replace-regexp-in-string "\"" "\\\\\"" full-prompt)
;;                                     temp-file)))
;;                 (error "Python script execution failed"))
;;               (with-temp-buffer
;;                 (insert-file-contents temp-file)
;;                 (buffer-string)))
;;           (ignore-errors
;;             (delete-file temp-file))))
;;     (error
;;      (elmo-log-prompt prompt)
;;      (elmo-log-error (format "Gemini API request failed.\n%s"
;;                              (error-message-string err)))
;;      nil)))

;; ;; (elmo-prompt-to-elisp-including-response-gemini "hello")

;; (defun elmo-prompt-to-elisp-including-response-claude (prompt)
;;   (condition-case err
;;       (let* ((full-prompt (elmo-prompt-combine "001-context-to-report" prompt))
;;              (url-request-method "POST")
;;              (url-request-extra-headers
;;               `(("Content-Type" . "application/json")
;;                 ("x-api-key" . ,elmo-anthropic-key)
;;                 ("anthropic-version" . "2023-06-01")))
;;              (url-request-data
;;               (encode-coding-string
;;                (json-encode
;;                 `(("model" . ,elmo-anthropic-engine)
;;                   ("max_tokens" . 8192)
;;                   ("system" . "Return only raw Elisp code without any markup or comments.")
;;                   ("messages" . [,(list (cons "role" "user")
;;                                         (cons "content" full-prompt))])))
;;                'utf-8))
;;              (buffer (url-retrieve-synchronously
;;                       "https://api.anthropic.com/v1/messages" t)))

;;         (if buffer
;;             (with-current-buffer buffer
;;               (goto-char (point-min))
;;               (let ((status-line (buffer-substring (point) (line-end-position))))
;;                 (if (string-match "200 OK" status-line)
;;                     (progn
;;                       (re-search-forward "^$")
;;                       (let ((json-object-type 'alist)
;;                             (json-array-type 'vector))
;;                         (condition-case nil
;;                             (let ((resp-data (json-read)))
;;                               (when resp-data
;;                                 (alist-get 'text (aref (alist-get 'content resp-data) 0))))
;;                           (error nil))))
;;                   (error "API request failed with status: %s" status-line))))
;;           (error "Failed to retrieve response")))
;;     (error
;;      (elmo-log-prompt prompt)
;;      (elmo-log-error (format "API request failed.\n%s"
;;                              (error-message-string err)))
;;      nil)))

;; ;; (elmo-prompt-to-elisp-including-response-claude "hello")

;; (defun elmo-extract-elisp-blocks (text)
;;   "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
;;   (let ((blocks nil)
;;         (start 0))
;;     (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
;;       (push (string-trim (match-string 1 text)) blocks)
;;       (setq start (match-end 0)))
;;     (if blocks
;;         (nreverse blocks)
;;       (error "No ELISP blocks found in response"))))

;; (defun elmo-lang2elisp (prompt)
;;   (let ((elisp-including-response nil)
;;         (elisp-blocks nil)
;;         (commands nil))
;;     (condition-case err
;;         (progn
;;           (setq elisp-including-response (elmo-prompt-to-elisp-including-response prompt))
;;           (unless elisp-including-response
;;             (signal 'elmo-api-error "No response received from API")))
;;       (error
;;        (elmo-log-prompt prompt)
;;        (elmo-log-error
;;         (format "API request failed.\n%s"
;;                 (error-message-string err)))
;;        (signal 'elmo-api-error err)))

;;     (when elisp-including-response
;;       (condition-case err
;;           (progn
;;             (setq elisp-blocks (elmo-extract-elisp-blocks elisp-including-response))
;;             (unless elisp-blocks
;;               (signal 'elmo-elisp-cleanup-error "No elisp blocks found in response")))
;;         (error
;;          (elmo-log-error
;;           (format "Elisp extraction failed.\n%s\n%s"
;;                   (error-message-string err) elisp-including-response))
;;          (signal 'elmo-elisp-cleanup-error err)))

;;       (condition-case err
;;           (progn
;;             (setq commands
;;                   (mapcar (lambda (block)
;;                             (read (concat "(progn " block ")")))
;;                           elisp-blocks))
;;             (unless commands
;;               (signal 'elmo-elisp-parse-error "No valid elisp code generated")))
;;         (error
;;          (elmo-log-error
;;           (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
;;                   (error-message-string err) elisp-blocks))
;;          (signal 'elmo-elisp-parse-error err))))
;;     (if commands
;;         (cons 'progn commands)
;;       (signal 'elmo-elisp-parse-error "No valid elisp code generated"))))

;; ;; (elmo-lang2elisp "hello")

;; (provide '09-elmo-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))