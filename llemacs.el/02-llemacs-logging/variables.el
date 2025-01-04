;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 09:31:25
;;; Time-stamp: <2025-01-04 09:31:25 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/variables.el

(defcustom llemacs--logging-level-threshold 'info
  "Minimum log level to record."
  :type 'symbol
  :group 'llemacs-logging)

(defcustom llemacs--logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs--logging-splitter "\n----------------------------------------\n"
  "Splitter between logs."
  :type 'string
  :group 'llemacs-logging)

;; (defcustom llemacs--logging-backup-count 5
;;   "Number of log backup files to keep."
;;   :type 'integer
;;   :group 'llemacs-logging)

;; (defun llemacs--logging-level-value (level)
;;   "Convert log LEVEL to numeric value for comparison."
;;   (pcase level
;;     ('debug 0)
;;     ('info 1)
;;     ('success 1)
;;     ('search 1)
;;     ('api 1)
;;     ('prompt 1)
;;     ('elisp 1)
;;     ('warn 2)
;;     ('success 3)
;;     ('error 4)
;;     (_ 1)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))