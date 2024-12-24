;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-25 00:54:44
;;; Time-stamp: <2024-12-25 00:54:44 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/09-elmo-lang2elisp.el

(require 'request)
(require '01-elmo-config)
(require '04-elmo-utils)
(require '02-elmo-logging-core)
(require '03-elmo-logging-utils)
(require '08-elmo-prompt-templates)

(defvar elmo-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar elmo-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude.")

;; (elmo-to-full-prompt "001-context-to-elisp-code" "hello")

;; ;; working
;; (defun elmo-to-full-prompt (prompt)
;;   (condition-case err
;;       (let* ((template (condition-case err1
;;                            (elmo-get-prompt "lang2elisp" "authorities" "logging" "notes")
;;                          (error
;;                           (elmo-log-error (format "Template fetch failed: %s" (error-message-string err1)))
;;                           nil)))
;;              (log-content (condition-case err2
;;                               (elmo-get-log)
;;                             (error
;;                              (elmo-log-error (format "Log fetch failed: %s" (error-message-string err2)))
;;                              nil))))
;;         (when template
;;           (condition-case err3
;;               (let ((prompt-with-template (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
;;                 (if log-content
;;                     (concat prompt-with-template "\n\n" log-content)
;;                   prompt-with-template))
;;             (error
;;              (elmo-log-error (format "Template substitution failed: %s" (error-message-string err3)))
;;              nil))))
;;     (error
;;      (elmo-log-error (format "Full prompt creation failed: %s" (error-message-string err)))
;;      nil)))

;; "authorities" "logging" "notes"
;; (elmo-to-full-prompt "Hello")


(defun elmo-prompt-to-elisp-including-response (prompt)
  (interactive)
  (condition-case err
      (let* ((full-prompt (elmo-to-full-prompt "001-context-to-elisp-code" prompt))
             (response (request
                         "https://api.anthropic.com/v1/messages"
                         :type "POST"
                         :headers `(("content-type" . "application/json")
                                    ("x-api-key" . ,elmo-anthropic-key)
                                    ("anthropic-version" . "2023-06-01"))
                         :data (json-encode
                                `(("model" . ,elmo-anthropic-engine)
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
     (elmo-log-error (format "API request failed.\nError: %s\nPrompt: %s"
                             (error-message-string err) prompt))
     nil)))

;; (elmo-prompt-to-elisp-including-response "plot a figure, save, and show on emacs")

(defun elmo-extract-elisp-blocks (text)
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
;; (elmo-extract-elisp-blocks (elmo-prompt-to-elisp-including-response "plot a figure, save, and show on emacs"))

;; (defun test-elisp-extraction ()
;;   (with-temp-buffer
;;     (insert-file-contents "/tmp/elispfile.elisp")
;;     (let ((content (buffer-string)))
;;       (elmo-extract-elisp-blocks content))))

;; ; (test-elisp-extraction)



;; (defun elmo-prompt-to-elisp (prompt)
(defun elmo-lang2elisp (prompt)
  (interactive)
  (let ((elisp-including-response nil)
        (elisp-blocks nil)
        (commands nil))
    (condition-case err
        (setq elisp-including-response (elmo-prompt-to-elisp-including-response prompt))
      (error
       (elmo-log-error
        (format "API request failed.\nError: %s\nPrompt: %s"
                (error-message-string err) prompt))
       (signal 'elmo-api-error err)))

    (when elisp-including-response
      (condition-case err
          (setq elisp-blocks (elmo-extract-elisp-blocks elisp-including-response))
        (error
         (elmo-log-error
          (format "Elisp extraction failed.\nError: %s\nResponse: %s"
                  (error-message-string err) elisp-including-response))
         (signal 'elmo-elisp-cleanup-error err)))

      (condition-case err
          (setq commands
                (mapcar (lambda (block)
                          (read (concat "(progn " block ")")))
                        elisp-blocks))
        (error
         (elmo-log-error
          (format "Elisp parsing failed.\nError: %s\nBlocks: %s"
                  (error-message-string err) elisp-blocks))
         (signal 'elmo-elisp-parse-error err)))

      (cons 'progn commands))))



;; (insert (format "%s" (elmo-lang2elisp "plot a figure, save, and show on emacs")))


(provide '09-elmo-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))