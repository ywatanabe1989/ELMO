;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 22:32:26
;;; Time-stamp: <2025-01-03 22:32:26 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/101-paths-logging-global.el

;; System Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories
(defcustom llemacs--path-logging-sys-logs
  (expand-file-name "logs" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-logs-by-level
  (expand-file-name "logs/by_level/" llemacs-path-workspace)
  "Directory for LLEMACS log files."
  :type 'directory
  :group 'llemacs-logging
  :group 'llemacs-sys)

(defcustom llemacs--path-logging-backups
  (expand-file-name "backups" llemacs--path-logging-sys-logs)
  "Directory for LLEMACS log backups."
  :type 'directory
  :group 'llemacs-logging
  :group 'llemacs-sys)


;; Global logging files
(defcustom llemacs--path-logging-sys-all
  (expand-file-name "logging.log" llemacs--path-logging-sys-logs)
  "Path to LLEMACS system log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


;; Level-specific logging files
(defcustom llemacs--path-logging-sys-debug
  (expand-file-name "debug.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system debug log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-info
  (expand-file-name "info.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system info log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-success
  (expand-file-name "success.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system success log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-prompt
  (expand-file-name "prompt.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system prompt log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-elisp
  (expand-file-name "elisp.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system elisp log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-api
  (expand-file-name "api.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system api log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-search
  (expand-file-name "search.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system search log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-warn
  (expand-file-name "warn.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system warn log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)


(defcustom llemacs--path-logging-sys-error
  (expand-file-name "error.log" llemacs--path-logging-sys-logs-by-level)
  "Path to LLEMACS system error log file."
  :type 'file
  :group 'llemacs-logging
  :group 'llemacs-sys)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))