;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 07:56:06
;;; Timestamp: <2025-01-11 07:56:06>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-llemacs-monitor/01-layout.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs-monitor (&optional pj-id)
  "Open monitoring layout for project in a new tab with management.png, reports and logs."
  (interactive)
  (let* ((pj-root (expand-file-name (or pj-id llemacs--cur-pj) llemacs--path-projects))
         (mmd-path (expand-file-name "project_management/project_management.png" pj-root))
         (logs-path (expand-file-name "logs" pj-root))
         (reports-path (expand-file-name "reports" pj-root))
         (mmd-buf (generate-new-buffer "*management*"))
         (logs-buf (generate-new-buffer "*logs*"))
         (reports-buf (generate-new-buffer "*reports*")))

    ;; (llemacs--pj-update-symlink pj-id)

    ;; Create new tab
    (tab-bar-new-tab)
    (tab-bar-rename-tab (or pj-id llemacs--cur-pj))

    ;; Split windows
    (delete-other-windows)
    (split-window-right)
    (split-window-below)

    ;; Show management.png upper left
    (set-buffer mmd-buf)
    (find-file mmd-path)
    (auto-revert-mode 1)
    (unless (eq major-mode 'image-mode)
      (error "Failed to enable image-mode"))

    ;; Show logs lower left
    (other-window 1)
    (set-buffer logs-buf)
    (find-file logs-path)
    (auto-revert-mode 1)

    ;; Show reports right
    (other-window 1)
    (set-buffer reports-buf)
    (find-file reports-path)
    (auto-revert-mode 1)))

;; (defun llemacs--monitor-wsl (&optional pj-id)
;;   ""
;;   (llemacs--pj-update-symlink pj-id)
;;   ;; management.png
;;   ;; reports
;;   ;; logs
;;   )

;; (llemacs--pj-update-symlink "103-epilepsty-seizure-detection")
(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
