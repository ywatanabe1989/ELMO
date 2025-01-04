;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 22:59:47
;;; Time-stamp: <2025-01-03 22:59:47 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/001-groups.el

;; Core Groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs nil
  "Llemacs: LLM Agents on Emacs"
  :group 'applications)

(defgroup llemacs-sys nil
  "System configurations for Llemacs."
  :group 'llemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feature Groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-path nil
  "Path configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-buffer nil
  "Buffer configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-logging nil
  "Logging configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-project nil
  "Project management for Llemacs."
  :group 'llemacs)

(defgroup llemacs-agent nil
  "Agent configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-script nil
  "Agent configurations for Llemacs."
  :group 'llemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-llm nil
  "LLM configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-search nil
  "Search configurations for Llemacs."
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))