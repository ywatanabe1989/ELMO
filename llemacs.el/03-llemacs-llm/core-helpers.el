;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:28:13
;;; Time-stamp: <2024-12-31 22:28:13 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/03-llemacs-llm-helpers.el

(defun llemacs--llm-api-check-rate-limit (provider)
  "Check if enough time has passed since last call to PROVIDER."
  (let ((last-call (gethash provider llemacs--llm-api-call-timestamps 0))
        (current-time (float-time)))
    (when (< (- current-time last-call) llemacs-api-min-delay)
      (signal 'llemacs--llm-api-rate-limit provider))
    (puthash provider current-time llemacs--llm-api-call-timestamps)))

;; API Error Handling
(define-error 'llemacs--llm-api-error "LLEMACS API Error")
(define-error 'llemacs--llm-api-rate-limit "API Rate Limit Exceeded")
(define-error 'llemacs--llm-api-auth-error "API Authentication Failed")

(defun llemacs--llm-api-handle-error (err provider)
  "Handle API error ERR from PROVIDER."
  (llemacs--logging-log-error
   (pcase (car err)
     ('llemacs--llm-api-rate-limit "LLM API Rate limit exceeded")
     ('llemacs--llm-api-auth-error "LLM API Authentication failed")
     (_ (format "LLM API error: %s" err)))))

(defun llemacs--llm-api-call-wrapper (provider func &rest args)
  "Wrap API call to PROVIDER using FUNC with ARGS."
  (llemacs--llm-api-check-rate-limit provider)
  (llemacs--logging-api-call provider (symbol-name func))
  (condition-case err
      (apply func args)
    (error
     (llemacs--llm-api-handle-error err provider)
     (signal 'llemacs--llm-api-error (list provider err)))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))