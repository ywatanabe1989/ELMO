;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 22:09:59
;;; Time-stamp: <2025-01-04 22:09:59 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/103-paths-pj-handlers.el

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
  (llemacs--path-create-or-update-log-paths-pj)

  ;; ;; Then ensure directories exist
  ;; (llemacs--path-pj-ensure-all) ;; fixme

  ;; Update buffer names
  (llemacs--pj-buf-update))

(defun llemacs--validate-pj-id (pj-id)
  "Validate PJ-ID format.
Returns error message if format is invalid, nil otherwise."
  (when (and (not (string-empty-p pj-id))
             (not (string-match-p "^[0-9]+-[a-zA-Z0-9-]+$" pj-id)))
    (format "Invalid project ID format. Expected '<id>-<project-name>' but got '%s'" pj-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getter & Setter (= Updater)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--pj-set-cur-pj (pj-id &optional force)
  "Set PJ-ID as the current active project."
  (llemacs--pj-lock-force-release pj-id force)
  (if-let ((err-msg (llemacs--validate-pj-id pj-id)))
      (error err-msg)
    (when-let ((lock-info (llemacs--pj-lock-check pj-id)))
      (error "Project is locked by %s. Only one user/process can switch to and work on a project at a time." lock-info))
    (setq llemacs--cur-pj pj-id)
    (llemacs--path-pj-update)
    (llemacs--pj-lock-acquire pj-id)))

(defalias 'llemacs--pj-switch
  'llemacs--pj-set-cur-pj
  "Set PJ-ID as the current active project.")

(defun llemacs--pj-get-cur-pj ()
  "Get currently active project ID."
  llemacs--cur-pj)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))