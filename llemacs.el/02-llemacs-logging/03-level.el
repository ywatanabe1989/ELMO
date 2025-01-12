;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:22:22
;;; Timestamp: <2025-01-11 08:22:22>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/level.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--logging-get-level-value (level)
  "Get numeric value for log LEVEL."
  (car (cdr (assq level llemacs--log-levels-sys))))

(defun llemacs--logging-should-log-p (level)
  "Check if LEVEL should be logged based on threshold."
  (>= (llemacs--logging-get-level-value level)
      (llemacs--logging-get-level-value llemacs--logging-level-threshold)))

(defun llemacs--logging-meets-main-log-threshold-p (level)
  "Check if LEVEL meets or exceeds the main.log threshold."
  (>= (cadr (assq level llemacs--log-levels-sys))
      (cadr (assq llemacs--logging-main-log-threshold
                  llemacs--log-levels-sys))))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
