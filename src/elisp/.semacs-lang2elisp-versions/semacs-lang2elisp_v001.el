;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-05 21:39:16
;;; Time-stamp: <2024-12-05 21:39:16 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-lang2elisp.el


(require 'ninja-config)
(require 'ninja-prompts)
(require 'ninja-utils)
(require 'ninja-version-control)
(require 'ninja-server)
(require 'ninja-logging)


(defun ninja-to-full-prompt (prompt)
  (condition-case err
      (let* ((template (ninja-get-prompt "lang2elisp" "authorities" "logging" "notes"))
             (log-content (ninja--get-log)))
        (when template
          (let ((prompt-with-template (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
            (if log-content
                (concat prompt-with-template "\n\n" log-content)
              prompt-with-template))))
    (error
     (ninja--log-error (format "Failed to create full prompt: %s" err))
     nil)))
 ;; "authorities" "logging" "notes"
;; (ninja-to-full-prompt "Hello")


(defun ninja-prompt2response (prompt)
  (interactive)
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
                               ("messages" . [,(list (cons "role" "user")
                                                   (cons "content" full-prompt))])))
                       :parser 'json-read
                       :sync t
                       :silent t))
             (resp-data (request-response-data response)))
        (when resp-data
          (alist-get 'text (aref (alist-get 'content resp-data) 0))))
    (error
     (ninja--log-error (format "API request failed: %s" err))
     nil)))
;; (ninja-prompt2response "Hello")

;; (defun ninja-prompt2response (prompt)
;;   (interactive)
;;   (setq request-message-level -1)
;;   (let* ((full-prompt (ninja-to-full-prompt prompt))
;;          (request-backend 'url-retrieve)
;;          (url "https://api.anthropic.com/v1/messages")
;;          (headers `(("content-type" . "application/json")
;;                    ("x-api-key" . ,ninja-anthropic-key)
;;                    ("anthropic-version" . "2023-06-01")))
;;          (data (json-encode
;;                 `(("model" . ,ninja-anthropic-engine)
;;                   ("max_tokens" . 8192)
;;                   ("messages" . [,(list (cons "role" "user")
;;                                       (cons "content" full-prompt))]))))
;;          (response (request
;;                    url
;;                    :type "POST"
;;                    :headers headers
;;                    :data data
;;                    :parser 'json-read
;;                    :sync t
;;                    :silent t))
;;          (resp-data (request-response-data response)))
;;     (when resp-data
;;       (alist-get 'text (aref (alist-get 'content resp-data) 0)))))

;; (ninja-prompt2response "hello world")
;; returns:
;; "```json
;; {
;; \"commands\": [
;; {
;; \"function\": \"message\",
;; \"args\": [\"Hello, world!\"]
;; }
;; ]
;; }
;; ```"

;; ;; no error feedback
;; (defun ninja--json-elisp-commands-to-elisp (json-data)
;;   (let* ((commands (alist-get 'commands json-data))
;;          (progn-form
;;           (cons 'progn
;;                 (mapcar
;;                  (lambda (cmd)
;;                    (let ((fn (alist-get 'function cmd))
;;                          (args (append (alist-get 'args cmd) nil)))
;;                      (cons (intern fn) args)))
;;                  commands))))
;;     progn-form))


(defun ninja--json-elisp-commands-to-elisp (json-data)
  (condition-case err
      (if (not (alist-get 'commands json-data))
          (error "No 'commands' field in JSON data: %s" json-data)
        (let* ((commands (alist-get 'commands json-data))
               (progn-form
                (cons 'progn
                      (mapcar
                       (lambda (cmd)
                         (unless (and (alist-get 'function cmd)
                                    (alist-get 'args cmd))
                           (error "Invalid command format: missing function or args in command: %s" cmd))
                         (let ((fn (alist-get 'function cmd))
                               (args (append (alist-get 'args cmd) nil)))
                           (cons (intern fn) args)))
                       commands))))
          progn-form))
    (error
     (ninja--log-error
      (format "JSON to Elisp conversion failed.\nError: %s\nInput JSON: %s"
              (error-message-string err)
              json-data))
     nil)))

;; (defun ninja--prompt-to-elisp (prompt)
;;   (interactive)
;;   (condition-case err
;;       (let* ((response-text (ninja-prompt2response prompt))
;;              (json-str-clean (replace-regexp-in-string "```json\\|```" "" response-text))
;;              (json-data (json-read-from-string json-str-clean)))
;;         (ninja--json-elisp-commands-to-elisp json-data))
;;     (error
;;      (ninja--log-error (format "Failed to convert prompt to elisp: %s\n\nResponse text: %s" err response-text))
;;      nil)))


(defun ninja--prompt-to-elisp (prompt)
  (interactive)
  (let ((response-text nil))
    (condition-case err
        (progn
          (setq response-text (ninja-prompt2response prompt))
          (let* ((json-str-clean (replace-regexp-in-string "```json\\|```" "" response-text))
                 (json-data (json-read-from-string json-str-clean)))
            (ninja--json-elisp-commands-to-elisp json-data)))
      (error
       (ninja--log-error (format "Failed to convert prompt to elisp: %s\n\nResponse text: %s" err response-text))
       nil))))

(provide 'ninja-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
