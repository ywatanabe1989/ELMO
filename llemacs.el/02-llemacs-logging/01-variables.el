;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:35:04
;;; Timestamp: <2025-01-11 08:35:04>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/variables.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--logging-level-threshold 'info
  "Minimum log level to record."
  :type 'symbol
  :group 'llemacs-logging)

(defcustom llemacs--logging-main-log-threshold 'info
  "Threshold level for including logs in main.log file."
  :type 'symbol
  :group 'llemacs-logging)

(defcustom llemacs--logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defvar llemacs--log-entry-limit 7
  "Maximum number of log entries to include in the context.")

(defcustom llemacs--logging-refresh-interval 5
  "Interval in seconds for auto-refreshing log buffer."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs--logging-splitter "\n----------------------------------------\n"
  "Splitter between logs."
  :type 'string
  :group 'llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
