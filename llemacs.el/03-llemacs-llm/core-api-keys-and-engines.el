;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:53:14
;;; Time-stamp: <2025-01-04 12:53:14 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-api-keys-and-engines.el

;; (defcustom llemacs--llm-api-keys-file
;;   (expand-file-name "api-keys.gpg" llemacs-path-home)
;;   "File storing encrypted API keys."
;;   :type 'file
;;   :group 'llemacs-llm)

(defcustom llemacs--llm-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-anthropic-engine (getenv "ANTHROPIC_ENGINE")
  "Model for Anthropic Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-google-key (getenv "GOOGLE_API_KEY")
  "API key for Google Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-google-engine (getenv "GOOGLE_ENGINE")
  "Model for Google Claude."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-deepseek-key (getenv "DEEPSEEK_API_KEY")
  "API key for DeepSeek."
  :type 'string
  :group 'llemacs-llm)

(defcustom llemacs--llm-deepseek-engine (getenv "DEEPSEEK_ENGINE")
  "Model for DeepSeek."
  :type 'string
  :group 'llemacs-llm)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))