;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 04:18:41
;;; Timestamp: <2025-01-11 04:18:41>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/06-pj-updater.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updater
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--path-pj-update ()
  "Update all project-related paths when switching projects."
  (unless llemacs--cur-pj
    (llemacs--logging-write-error-pj "No project selected"))
  ;; Set all path variables first
  (setq llemacs--path-pj (expand-file-name llemacs--cur-pj llemacs--path-projects))
  (setq llemacs--path-pj-root llemacs--path-pj)
  (setq llemacs--path-pj-lock (expand-file-name ".lock" llemacs--path-pj))
  (setq llemacs--path-pj-config (expand-file-name "config" llemacs--path-pj))
  (setq llemacs--path-pj-data (expand-file-name "data" llemacs--path-pj))
  (setq llemacs--path-pj-results (expand-file-name "results" llemacs--path-pj))
  (setq llemacs--path-pj-results-figures (expand-file-name "figures" llemacs--path-pj-results))
  (setq llemacs--path-pj-results-tables (expand-file-name "tables" llemacs--path-pj-results))
  (setq llemacs--path-pj-scripts (expand-file-name "scripts" llemacs--path-pj))
  (setq llemacs--path-pj-logs (expand-file-name "logs" llemacs--path-pj))
  (setq llemacs--path-pj-logs-backup (expand-file-name "backup" llemacs--path-pj-logs))
  (setq llemacs--path-pj-logs-all (expand-file-name "all.log" llemacs--path-pj-logs))
  (setq llemacs--path-pj-logs-by-level (expand-file-name "by_level" llemacs--path-pj-logs))
  (setq llemacs--path-pj-pm-mmd (expand-file-name "project_management/project_management.mmd" llemacs--path-pj))
  (setq llemacs--path-pj-python-env (expand-file-name ".env" llemacs--path-pj))
  (setq llemacs--path-pj-python (expand-file-name "bin/python" llemacs--path-pj-python-env))
  (setq llemacs--path-pj-reports (expand-file-name "reports" llemacs--path-pj))
  (llemacs--path-logs-update-pj)
  ;; Update buffer names
  (llemacs--pj-buf-update))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
