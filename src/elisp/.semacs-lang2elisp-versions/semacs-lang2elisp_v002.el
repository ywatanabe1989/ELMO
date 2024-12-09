;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-05 21:51:00
;;; Time-stamp: <2024-12-05 21:51:00 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-lang2elisp.el


(require 'semacs-config)
(require 'semacs-prompts)
(require 'semacs-utils)
(require 'semacs-version-control)
(require 'semacs-server)
(require 'semacs-logging)


(defun semacs-to-full-prompt (prompt)
  (condition-case err
      (let* ((template (semacs-get-prompt "lang2elisp" "authorities" "logging" "notes"))
             (log-content (semacs--get-log)))
        (when template
          (let ((prompt-with-template (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
            (if log-content
                (concat prompt-with-template "\n\n" log-content)
              prompt-with-template))))
    (error
     (semacs--log-error (format "Failed to create full prompt.\nError: %s\nPrompt: %s"
                           (error-message-string err) prompt))
     nil)))
 ;; "authorities" "logging" "notes"
;; (semacs-to-full-prompt "Hello")


(defun semacs-prompt2response (prompt)
  (interactive)
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
;; (semacs-prompt2response "hello world")
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

(defun semacs--json-elisp-commands-to-elisp (json-data)
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
     (semacs--log-error
      (format "JSON to Elisp conversion failed.\nError: %s\nInput JSON: %s"
              (error-message-string err)
              json-data))
     nil)))

;; (defun semacs--prompt-to-elisp (prompt)
;;   (interactive)
;;   (let ((response-text nil))
;;     (condition-case err
;;         (progn
;;           (setq response-text (semacs-prompt2response prompt))
;;           (let* ((json-str-clean (replace-regexp-in-string "```json\\|```" "" response-text))
;;                  (json-data (json-read-from-string json-str-clean)))
;;             (semacs--json-elisp-commands-to-elisp json-data)))
;;       (error
;;        (semacs--log-error
;;         (format "Failed to convert prompt to elisp.\nError: %s\nPrompt: %s\nResponse: %s"
;;                 (error-message-string err) prompt response-text))
;;        nil))))

(defun semacs--prompt-to-elisp (prompt)
  (interactive)
  (let ((response-text nil)
        (json-str-clean nil)
        (json-data nil))
    (condition-case err
        (setq response-text (semacs-prompt2response prompt))
      (error
       (semacs--log-error
        (format "API request failed.\nError: %s\nPrompt: %s"
                (error-message-string err) prompt))
       (signal 'semacs-api-error err)))

    (when response-text
      (condition-case err
          (setq json-str-clean
                (replace-regexp-in-string "```json\\|```" "" response-text))
        (error
         (semacs--log-error
          (format "JSON string cleanup failed.\nError: %s\nResponse: %s"
                  (error-message-string err) response-text))
         (signal 'semacs-json-cleanup-error err)))

      (condition-case err
          (setq json-data (json-read-from-string json-str-clean))
        (error
         (semacs--log-error
          (format "JSON parsing failed.\nError: %s\nCleaned JSON: %s"
                  (error-message-string err) json-str-clean))
         (signal 'semacs-json-parse-error err)))

      (condition-case err
          (semacs--json-elisp-commands-to-elisp json-data)
        (error
         (semacs--log-error
          (format "Command conversion failed.\nError: %s\nJSON Data: %s"
                  (error-message-string err) json-data))
         (signal 'semacs-command-conversion-error err))))))

(provide 'semacs-lang2elisp)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
