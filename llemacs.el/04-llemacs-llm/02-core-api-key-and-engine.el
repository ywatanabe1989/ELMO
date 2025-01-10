;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 05:09:39
;;; Timestamp: <2025-01-11 05:09:39>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/02-core-api-key-and-engine.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--llm-provider "google"
  "Switcher for LLM provider"
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-key-anthropic (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-engine-anthropic (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-key-google (getenv "GOOGLE_API_KEY")
  "API key for Google Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-engine-google (getenv "GOOGLE_ENGINE")
  "Model for Google Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-key-deepseek (getenv "DEEPSEEK_API_KEY")
  "API key for DeepSeek."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-engine-deepseek (getenv "DEEPSEEK_ENGINE")
  "Model for DeepSeek."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-api-key-groq (getenv "GROQ_API_KEY")
  "API key for Groq."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-engine-groq (getenv "GROQ_ENGINE")
  "Model for Groq."
  :type 'string
  :group 'llemacs-llm)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
