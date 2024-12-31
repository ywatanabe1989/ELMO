;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:45:25
;;; Time-stamp: <2024-12-31 22:45:25 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/03-llemacs-llm-call.el

(defun llemacs-llm (prompt &optional template)
  "Process PROMPT using configured LLM provider.
Optional TEMPLATE is used to combine with prompt."
  (interactive)
  (let ((text (if template
                  (llemacs-to-full-prompt template prompt)
                prompt)))
    (pcase llemacs-llm-provider
      ("anthropic" (llemacs--llm-claude text))
      ("google" (llemacs--llm-gemini text))
      ("deepseek" (llemacs--llm-deepseek text))
      (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))

;; (defun llemacs-llm (prompt &optional template)
;;   "Process PROMPT using configured LLM provider.
;; Optional TEMPLATE is used to combine with prompt."
;;   (let ((text (if template
;;                   (llemacs-to-full-prompt template prompt)
;;                 prompt)))
;;     (pcase llemacs-llm-provider
;;       ("anthropic" (llemacs--llm-claude text))
;;       ("google" (llemacs--llm-gemini text))
;;       ("deepseek" (llemacs--llm-deepseek text))
;;       (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))

(provide '03-llemacs-llm-call)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))