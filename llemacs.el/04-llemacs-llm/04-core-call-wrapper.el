;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:26:35
;;; Time-stamp: <2025-01-06 17:26:35 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-call-wrapper.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

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
    (format "%s" response)
    ))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
