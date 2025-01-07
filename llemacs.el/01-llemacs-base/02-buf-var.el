;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 09:35:24
;;; Time-stamp: <2025-01-06 09:35:24 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/010-buf-var.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:49:29
;;; Time-stamp: <2025-01-04 14:49:29 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/010-buf-var.el

(defvar llemacs--tab-counter 0
  "Counter for LLEMACS tab numbering.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--buf-main-sys
  "*LLEMACS-MAIN*"
  "Name of main buffer."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-sys)

(defcustom llemacs--buf-logging-sys
  "*LLEMACS-LOGGING*"
  "Name of log buffer."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-logging
  :group 'llemacs-sys)

(defcustom llemacs--buf-debug-sys
  "*LLEMACS-DEBUG*"
  "Buffer for debug output when debug mode enabled."
  :type 'string
  :group 'llemacs-sys
  :group 'llemacs-buffer
  :group 'llemacs-logging)

(defcustom llemacs--buf-prompt-sys
  "*LLEMACS-PROMPT*"
  "Buffer for prompt operations."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-llm
  :group 'llemacs-sys)

(defcustom llemacs--buf-search-sys
  "*LLEMACS-SEARCH*"
  "Buffer for search results."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-search
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PJ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--buf-main-pj
  (format "*LLEMACS-MAIN (%s)*" "")
  "Name of main buffer."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-project)

(defcustom llemacs--buf-logging-pj
  (format "*LLEMACS-LOGGING (%s)*" "")
  "Name of log buffer."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-logging
  :group 'llemacs-project)

(defcustom llemacs--buf-debug-pj
  (format "*LLEMACS-DEBUG (%s)*" "")
  "Buffer for debug output when debug mode enabled."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-logging
  :group 'llemacs-project)

(defcustom llemacs--buf-prompt-pj
  (format "*LLEMACS-PROMPT (%s)*" "")
  "Buffer for prompt operations."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-llm
  :group 'llemacs-project)

(defcustom llemacs--buf-search-pj
  (format "*LLEMACS-SEARCH (%s)*" "")
  "Buffer for search results."
  :type 'string
  :group 'llemacs-buffer
  :group 'llemacs-search
  :group 'llemacs-project)

(defun llemacs--buf-update-pj ()
  "Update project-specific buffer names."
  (let ((pj-id (llemacs--pj-get-cur-pj)))
    (set 'llemacs--buf-main-pj (format "*LLEMACS-MAIN (%s)*" pj-id))
    (set 'llemacs--buf-logging-pj (format "*LLEMACS-LOGGING (%s)*" pj-id))
    (set 'llemacs--buf-debug-pj (format "*LLEMACS-DEBUG (%s)*" pj-id))
    (set 'llemacs--buf-prompt-pj (format "*LLEMACS-PROMPT (%s)*" pj-id))
    (set 'llemacs--buf-search-pj (format "*LLEMACS-SEARCH (%s)*" pj-id))))

(defalias 'llemacs--pj-buf-update 'llemacs--buf-update-pj
  "Update project-specific buffer names.")

(defalias 'llemacs--pj-buf-main 'llemacs--buf-main-pj
  "Buffer name for project main buffer.")

(defalias 'llemacs--pj-buf-logging 'llemacs--buf-logging-pj
  "Buffer name for project logging.")

(defalias 'llemacs--pj-buf-debug 'llemacs--buf-debug-pj
  "Buffer name for project debug output.")

(defalias 'llemacs--pj-buf-prompt 'llemacs--buf-prompt-pj
  "Buffer name for project prompt operations.")

(defalias 'llemacs--pj-buf-search 'llemacs--buf-search-pj
  "Buffer name for project search results.")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))