;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 07:55:21
;;; Timestamp: <2025-01-11 07:55:21>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-llemacs-monitor/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-monitoring-components ()
  "Load LLM component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "01-layout.el" dir))
    ))

;; Initialize Monitoring components
(llemacs--load-monitoring-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
