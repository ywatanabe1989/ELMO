;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 08:50:07
;;; Timestamp: <2025-01-10 08:50:07>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/01-groups.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Project management configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-agent nil
  "Agent configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-script nil
  "Agent configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-run nil
  "Configurations for running the core functions of Llemacs."
  :group 'llemacs)

(defgroup llemacs-secret nil
  "Sensitive configurations for Llemacs."
  :group 'llemacs)

(defgroup llemacs-git nil
  "Git operations for Llemacs."
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
