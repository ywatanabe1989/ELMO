;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 09:10:42
;;; Time-stamp: <2025-01-06 09:10:42 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/104-paths-pj-updater.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--path-pj-update ()
  "Update all project-related paths when switching projects."
  (unless llemacs--cur-pj
    (error "No project selected"))

  ;; Set all path variables first
  (set 'llemacs--path-pj (expand-file-name llemacs--cur-pj llemacs--path-projects))
  (set 'llemacs--path-pj-lock (expand-file-name ".lock" llemacs--path-pj))
  (set 'llemacs--path-pj-config (expand-file-name "config" llemacs--path-pj))
  (set 'llemacs--path-pj-data (expand-file-name "data" llemacs--path-pj))
  (set 'llemacs--path-pj-results (expand-file-name "results" llemacs--path-pj))
  (set 'llemacs--path-pj-scripts (expand-file-name "scripts" llemacs--path-pj))
  (set 'llemacs--path-pj-logs (expand-file-name "logs" llemacs--path-pj))
  (set 'llemacs--path-pj-logs-all (expand-file-name "all.log" llemacs--path-pj-logs))
  (set 'llemacs--path-pj-logs-by-level (expand-file-name "by_level" llemacs--path-pj-logs))
  (set 'llemacs--path-pj-pm-mmd (expand-file-name "project_management/project_management.mmd" llemacs--path-pj))
  (set 'llemacs--path-python-env-pj (expand-file-name ".env" llemacs--path-pj))
  (set 'llemacs--path-python (expand-file-name "bin/python" llemacs--path-python-env-pj))
  (llemacs--path-logs-update-pj)

  ;; Update buffer names
  (llemacs--pj-buf-update))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))