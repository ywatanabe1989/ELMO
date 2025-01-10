;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:23:23
;;; Time-stamp: <2025-01-06 17:23:23 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/getters.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


(defun llemacs--logging-get-log-entries (file-path)
  "Get log entries from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (split-string (buffer-string) "\n" t))))

(defun llemacs--logging-get-logs (&optional level is-pj)
  "Get logs based on LEVEL and IS-PJ flags."
  (cond
   ;; Get logs by level (project or system)
   (level
    (let* ((var-name (intern (format "llemacs--path-%slogs-%s"
                                     (if is-pj "pj-" "logs-")
                                     (if is-pj
                                         level
                                       (concat (symbol-name level) "-sys")))))
           (log-file (symbol-value var-name)))
      (llemacs--logging-get-log-entries log-file)))
   ;; Get all logs (project or system)
   (t
    (llemacs--logging-get-log-entries
     (if is-pj llemacs--path-pj-logs-all llemacs--path-logs-all-sys)))))


(defun llemacs--logging-get-logs-sys (&optional level)
  "Get system logs."
  (llemacs--logging-get-logs level nil))

(defun llemacs--logging-get-logs-pj (&optional level)
  "Get project logs."
  (llemacs--logging-get-logs level nil))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
