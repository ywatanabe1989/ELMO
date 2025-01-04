;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 19:49:21
;;; Time-stamp: <2025-01-03 20:02:30 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/001-2nd-groups.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-path-sys nil
  "System-level path configurations for Llemacs."
  :group 'llemacs-sys)

(defgroup llemacs-path-proj nil
  "Project-level path configurations for Llemacs."
  :group 'llemacs-proj)

(defgroup llemacs-path-agent nil
  "Agent-level path configurations for Llemacs."
  :group 'llemacs-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-buffer-sys nil
  "System-level buffer configurations for Llemacs."
  :group 'llemacs-sys)

(defgroup llemacs-buffer-proj nil
  "Project-level buffer configurations for Llemacs."
  :group 'llemacs-proj)

(defgroup llemacs-buffer-agent nil
  "Agent-level buffer configurations for Llemacs."
  :group 'llemacs-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-milestone nil
  "Milestone management for Llemacs."
  :group 'llemacs-proj)

(defgroup llemacs-task nil
  "Task management for Llemacs."
  :group 'llemacs-proj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-logging-sys nil
  "System-level logging configurations for Llemacs."
  :group 'llemacs-sys)

(defgroup llemacs-logging-proj nil
  "Project-level logging configurations for Llemacs."
  :group 'llemacs-proj)

(defgroup llemacs-logging-agent nil
  "Agent-level logging configurations for Llemacs."
  :group 'llemacs-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Externals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup llemacs-llm nil
  "LLM configurations for Llemacs."
  :group 'llemacs-ext)

(defgroup llemacs-search nil
  "Search configurations for Llemacs."
  :group 'llemacs-ext)

;; (apropos "^llemacs-path" 'custom-group)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
