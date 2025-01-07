;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:51:16
;;; Time-stamp: <2025-01-01 22:51:16 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/configs.el

(defcustom llemacs--llm-api-timeout 30
  "Default timeout for API requests in seconds."
  :type 'integer
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-retries 3
  "Number of retries for failed API requests."
  :type 'integer
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-endpoints
  '((openai . "https://api.openai.com/v1")
    (anthropic . "https://api.anthropic.com")
    (cohere . "https://api.cohere.ai/v1"))
  "API endpoints for different LLM providers."
  :type '(alist :key-type symbol :value-type string)
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-min-delay 1.0
  "Minimum delay between API calls in seconds."
  :type 'float
  :group 'llemacs-llm)


(defvar llemacs--llm-api-call-timestamps (make-hash-table :test 'equal)
  "Hash table tracking API call timestamps per provider.")

(custom-declare-variable 'llemacs--llm-api-call-timestamps
                         (make-hash-table :test 'equal)
                         "Hash table tracking API call timestamps per provider."
                         :type 'string
                         :group 'llemacs-llm)

;; (defvar llemacs--llm-api-call-timestamps (make-hash-table :test 'equal)
;;   "Hash table tracking API call timestamps per provider."
;;   :type 'string
;;   :group 'llemacs-llm)

;; (custom-declare-variable 'llemacs--llm-api-call-timestamps
;;                          (make-hash-table :test 'equal)
;;                          "Hash table tracking API call timestamps per provider."
;;                          :type 'string
;;                          :group 'llemacs-llm)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))