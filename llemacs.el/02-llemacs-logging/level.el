;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 21:53:57
;;; Time-stamp: <2025-01-04 21:53:57 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/level.el

(defun llemacs--logging-get-level-value (level)
  "Get numeric value for log LEVEL."
  (car (cdr (assq level llemacs--log-levels-sys))))

(defun llemacs--logging-should-log-p (level)
  "Check if LEVEL should be logged based on threshold."
  (>= (llemacs--logging-get-level-value level)
      (llemacs--logging-get-level-value llemacs--logging-level-threshold)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))