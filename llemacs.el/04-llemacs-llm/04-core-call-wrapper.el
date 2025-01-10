;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 21:38:56
;;; Timestamp: <2025-01-10 21:38:56>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/04-core-call-wrapper.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs-llm (prompt &optional recipe-id)
  "Process PROMPT using configured LLM provider."
  (interactive "sPrompt: ")
  (let* ((text (if recipe-id
                   (llemacs--llm-prompt-embed prompt recipe-id)
                 prompt))
         (raw-response (pcase llemacs-llm-provider
                         ("anthropic" (llemacs--llm-claude text))
                         ("google" (llemacs--llm-gemini text))
                         ("deepseek" (llemacs--llm-deepseek text))
                         (_ (llemacs--logging-write-error-pj "Unknown LLM provider: %s" llemacs-llm-provider))))
         (response (replace-regexp-in-string "[^[:ascii:]]" "" raw-response)))
    (when (called-interactively-p 'any)
      (message "%s" response))
    response))

;; (llemacs--llm-prompt-embed "plot something" "code-elisp-progn")
;; (llemacs-llm "plot something" "code-elisp-progn")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
