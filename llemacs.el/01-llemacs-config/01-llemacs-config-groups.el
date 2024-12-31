;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 20:13:51
;;; Time-stamp: <2024-12-31 20:13:51 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-config/01-llemacs-config-groups.el

;; ----------------------------------------
;; Groups
;; ----------------------------------------
;; Core
(defgroup llemacs nil
  "LLM Agents on Emacs"
  :group 'applications)

(defgroup llemacs-system nil
  "System configurations for Llemacs."
  :group 'llemacs)

;; System
(defgroup llemacs-path nil
  "Path configurations."
  :group 'llemacs-system)

(defgroup llemacs-buffer nil
  "Buffer configurations."
  :group 'llemacs-system)

(defgroup llemacs-logging nil
  "Logging configurations."
  :group 'llemacs-system)

;; Features
(defgroup llemacs-llm nil
  "LLM configurations."
  :group 'llemacs)

(defgroup llemacs-search nil
  "Search configurations."
  :group 'llemacs)

;; Project Management
(defgroup llemacs-project nil
  "Project management."
  :group 'llemacs)

(defgroup llemacs-milestone nil
  "Milestone management."
  :group 'llemacs-project)

(defgroup llemacs-task nil
  "Task management."
  :group 'llemacs-project)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))