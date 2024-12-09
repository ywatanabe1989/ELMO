;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 11:33:33
;;; Time-stamp: <2024-12-07 11:33:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-lang2elisp.el


(require 'request)
(require 'semacs-config)
(require 'semacs-utils)
(require 'semacs-logging)
(require 'semacs-prompts)
(require 'semacs-server)
(require 'semacs-version-control)

;; working
(defun semacs-to-full-prompt (prompt)
  (condition-case err
      (let* ((template (condition-case err1
                          (semacs-get-prompt "lang2elisp" "authorities" "logging" "notes")
                        (error
                         (semacs--log-error (format "Template fetch failed: %s" (error-message-string err1)))
                         nil)))
             (log-content (condition-case err2
                             (semacs--get-log)
                           (error
                            (semacs--log-error (format "Log fetch failed: %s" (error-message-string err2)))
                            nil))))
        (when template
          (condition-case err3
              (let ((prompt-with-template (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
                (if log-content
                    (concat prompt-with-template "\n\n" log-content)
                  prompt-with-template))
            (error
             (semacs--log-error (format "Template substitution failed: %s" (error-message-string err3)))
             nil))))
    (error
     (semacs--log-error (format "Full prompt creation failed: %s" (error-message-string err)))
     nil)))

 ;; "authorities" "logging" "notes"
;; (semacs-to-full-prompt "Hello")


(defun semacs-prompt2response (prompt)
  (condition-case err
      (let* ((full-prompt (semacs-to-full-prompt prompt))
             (response (request
                       "https://api.anthropic.com/v1/messages"
                       :type "POST"
                       :headers `(("content-type" . "application/json")
                                ("x-api-key" . ,semacs-anthropic-key)
                                ("anthropic-version" . "2023-06-01"))
                       :data (json-encode
                             `(("model" . ,semacs-anthropic-engine)
                               ("max_tokens" . 8192)
                               ("system" . "Return only raw Elisp code without any markup or comments.")
                               ("messages" . [,(list (cons "role" "user")
                                                   (cons "content" full-prompt))])))
                       :parser 'json-read
                       :sync t
                       :silent t))
             (resp-data (request-response-data response)))
        (when resp-data
          (alist-get 'text (aref (alist-get 'content resp-data) 0))))
    (error
     (semacs--log-error (format "API request failed.\nError: %s\nPrompt: %s"
                           (error-message-string err) prompt))
     nil)))


(defun semacs--extract-elisp-blocks (text)
  "Extract all ELISP blocks between ```elisp and ``` markers from TEXT."
  (interactive)
  (let ((blocks nil)
        (start 0))
    (while (string-match "```elisp\n\\(\\(?:.\\|\n\\)*?\\)\n```" text start)
      (push (string-trim (match-string 1 text)) blocks)
      (setq start (match-end 0)))
    (if blocks
        (nreverse blocks)
      (error "No ELISP blocks found in response"))))
;; (semacs--extract-elisp-blocks (semacs-prompt2response "hi"))



(defun test-elisp-extraction ()
  (with-temp-buffer
    (insert-file-contents "/tmp/elispfile.elisp")
    (let ((content (buffer-string)))
      (semacs--extract-elisp-blocks content))))

; (test-elisp-extraction)



(defun semacs--prompt-to-elisp (prompt)
  (interactive)
  (let ((response-text nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (setq response-text (semacs-prompt2response prompt))
      (error
       (semacs--log-error
        (format "API request failed.\nError: %s\nPrompt: %s"
                (error-message-string err) prompt))
       (signal 'semacs-api-error err)))

    (when response-text
      (condition-case err
          (setq elisp-blocks (semacs--extract-elisp-blocks response-text))
        (error
         (semacs--log-error
          (format "Elisp extraction failed.\nError: %s\nResponse: %s"
                  (error-message-string err) response-text))
         (signal 'semacs-elisp-cleanup-error err)))

      (condition-case err
          (setq commands
                (mapcar (lambda (block)
                         (read (concat "(progn " block ")")))
                       elisp-blocks))
        (error
         (semacs--log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'semacs-elisp-parse-error err)))

      (cons 'progn commands))))

; (insert (format "%s" (semacs--prompt-to-elisp "write a sipmle python code")))


(provide 'semacs-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
