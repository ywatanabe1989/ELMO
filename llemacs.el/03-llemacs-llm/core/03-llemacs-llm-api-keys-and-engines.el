;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:27:23
;;; Time-stamp: <2024-12-31 22:27:23 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/core/03-llemacs-llm-api-keys-and-engines.el

(defcustom llemacs--llm-api-keys-file
  (expand-file-name "api-keys.gpg" llemacs-path-home)
  "File storing encrypted API keys."
  :type 'file
  :group 'llemacs-llm)

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