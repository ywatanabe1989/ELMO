;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-07 11:33:33
;;; Time-stamp: <2024-12-07 11:33:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-lang2elisp.el


(require 'request)
(require 'ninja-config)
(require 'ninja-utils)
(require 'ninja-logging)
(require 'ninja-prompts)
(require 'ninja-server)
(require 'ninja-version-control)

;; working
(defun ninja-to-full-prompt (prompt)
  (condition-case err
      (let* ((template (condition-case err1
                          (ninja-get-prompt "lang2elisp" "authorities" "logging" "notes")
                        (error
                         (ninja--log-error (format "Template fetch failed: %s" (error-message-string err1)))
                         nil)))
             (log-content (condition-case err2
                             (ninja--get-log)
                           (error
                            (ninja--log-error (format "Log fetch failed: %s" (error-message-string err2)))
                            nil))))
        (when template
          (condition-case err3
              (let ((prompt-with-template (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
                (if log-content
                    (concat prompt-with-template "\n\n" log-content)
                  prompt-with-template))
            (error
             (ninja--log-error (format "Template substitution failed: %s" (error-message-string err3)))
             nil))))
    (error
     (ninja--log-error (format "Full prompt creation failed: %s" (error-message-string err)))
     nil)))

 ;; "authorities" "logging" "notes"
;; (ninja-to-full-prompt "Hello")


(defun ninja-prompt2response (prompt)
  (condition-case err
      (let* ((full-prompt (ninja-to-full-prompt prompt))
             (response (request
                       "https://api.anthropic.com/v1/messages"
                       :type "POST"
                       :headers `(("content-type" . "application/json")
                                ("x-api-key" . ,ninja-anthropic-key)
                                ("anthropic-version" . "2023-06-01"))
                       :data (json-encode
                             `(("model" . ,ninja-anthropic-engine)
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
     (ninja--log-error (format "API request failed.\nError: %s\nPrompt: %s"
                           (error-message-string err) prompt))
     nil)))


(defun ninja--extract-elisp-blocks (text)
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
;; (ninja--extract-elisp-blocks (ninja-prompt2response "hi"))



(defun test-elisp-extraction ()
  (with-temp-buffer
    (insert-file-contents "/tmp/elispfile.elisp")
    (let ((content (buffer-string)))
      (ninja--extract-elisp-blocks content))))

; (test-elisp-extraction)



(defun ninja--prompt-to-elisp (prompt)
  (interactive)
  (let ((response-text nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (setq response-text (ninja-prompt2response prompt))
      (error
       (ninja--log-error
        (format "API request failed.\nError: %s\nPrompt: %s"
                (error-message-string err) prompt))
       (signal 'ninja-api-error err)))

    (when response-text
      (condition-case err
          (setq elisp-blocks (ninja--extract-elisp-blocks response-text))
        (error
         (ninja--log-error
          (format "Elisp extraction failed.\nError: %s\nResponse: %s"
                  (error-message-string err) response-text))
         (signal 'ninja-elisp-cleanup-error err)))

      (condition-case err
          (setq commands
                (mapcar (lambda (block)
                         (read (concat "(progn " block ")")))
                       elisp-blocks))
        (error
         (ninja--log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'ninja-elisp-parse-error err)))

      (cons 'progn commands))))

; (insert (format "%s" (ninja--prompt-to-elisp "write a sipmle python code")))


(provide 'ninja-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
