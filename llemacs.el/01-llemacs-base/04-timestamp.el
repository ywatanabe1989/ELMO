;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 18:46:58
;;; Time-stamp: <2025-01-06 18:46:58 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/99-timestamp.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--timestamp-format
  "%Y-%m%d-%H%M%S"
  "Timestamp references by Llemacs."
  :type 'string
  :group 'llemacs-sys)

(defcustom llemacs--timestamp
  (format-time-string llemacs--timestamp-format)
  "Timestamp references by Llemacs."
  :type 'string
  :group 'llemacs-sys)

(defun llemacs--timestamp-get ()
  "Get current timestamp without updating the reference."
  (format-time-string llemacs--timestamp-format))

(defalias 'llemacs-timestamp 'llemacs--timestamp-get)

(defun llemacs--timestamp-set ()
  "Set the reference timestamp."
  (setq llemacs--timestamp (llemacs--timestamp-get)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
