;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 23:15:02
;;; Time-stamp: <2025-01-04 23:15:02 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-call-wrapper.el

;; (defun llemacs-llm (prompt &optional template)
;;   "Process PROMPT using configured LLM provider."
;;   (interactive "sPrompt: ")
;;   (let* ((text (if template
;;                    (llemacs--llm-prompt-embed template prompt)
;;                  prompt))
;;          (response (pcase llemacs-llm-provider
;;                      ("anthropic" (llemacs--llm-claude text))
;;                      ("google" (llemacs--llm-gemini text))
;;                      ("deepseek" (llemacs--llm-deepseek text))
;;                      (_ (error "Unknown LLM provider: %s" llemacs-llm-provider)))))
;;     (when (called-interactively-p 'any)
;;       (message "%s" response))
;;     response))


(defun llemacs-llm (prompt &optional template)
  "Process PROMPT using configured LLM provider."
  (interactive "sPrompt: ")
  (let* ((text (if template
                   (llemacs--llm-prompt-embed template prompt)
                 prompt))
         (raw-response (pcase llemacs-llm-provider
                         ("anthropic" (llemacs--llm-claude text))
                         ("google" (llemacs--llm-gemini text))
                         ("deepseek" (llemacs--llm-deepseek text))
                         (_ (error "Unknown LLM provider: %s" llemacs-llm-provider))))
         (response (replace-regexp-in-string "[^[:ascii:]]" "" raw-response)))
    (when (called-interactively-p 'any)
      (message "%s" response))
    ;; response
    (format "%s" response)
    ))

;; M-x llemacs-llm RET hi ;; this does not work
;; (llemacs-llm "hello") ;; this returns message

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))