;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:47:54
;;; Time-stamp: <2025-01-01 22:47:54 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/03-llemacs-llm-call.el

(defun llemacs-llm (prompt &optional template)
  "Process PROMPT using configured LLM provider."
  (interactive "sPrompt: ")
  (let* ((text (if template
                   (llemacs-to-full-prompt template prompt)
                 prompt))
         (response (pcase llemacs-llm-provider
                     ("anthropic" (llemacs--llm-claude text))
                     ("google" (llemacs--llm-gemini text))
                     ("deepseek" (llemacs--llm-deepseek text))
                     (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))
    (when (called-interactively-p 'any)
      (message "%s" response))
    response))

;; (defun llemacs-llm (prompt &optional template)
;;   "Process PROMPT using configured LLM provider."
;;   (interactive "sPrompt: ")
;;   (let ((text (if template
;;                   (llemacs-to-full-prompt template prompt)
;;                 prompt)))
;;     (pcase llemacs-llm-provider
;;       ("anthropic" (llemacs--llm-claude text))
;;       ("google" (llemacs--llm-gemini text))
;;       ("deepseek" (llemacs--llm-deepseek text))
;;       (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))

;; M-x llemacs-llm RET hi ;; this does not work
;; (llemacs-llm "hello") ;; this returns message

(provide '03-llemacs-llm-call)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))