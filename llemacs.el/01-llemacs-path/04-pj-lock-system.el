;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 09:02:05
;;; Time-stamp: <2025-01-06 09:02:05 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/102-paths-pj-lock-system.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lock System; Only one user/process can switch to and work on a project at a time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--pj-lock-path (pj-id)
  "Get lock file path for PJ-ID."
  (expand-file-name ".lock" (expand-file-name pj-id llemacs--path-projects)))

(defun llemacs--pj-lock-acquire (pj-id)
  "Try to acquire project lock for PJ-ID. Return t if successful."
  (condition-case nil
      (progn
        (write-region (format "%s@%s" (user-login-name) (system-name))
                      nil (llemacs--pj-lock-path pj-id) nil 'quiet)
        t)
    (file-error nil)))

(defun llemacs--pj-lock-release (pj-id)
  "Release project lock for PJ-ID."
  (when (file-exists-p (llemacs--pj-lock-path pj-id))
    (delete-file (llemacs--pj-lock-path pj-id))))

(defun llemacs--pj-lock-check (pj-id)
  "Check if project PJ-ID is locked by another process."
  (when (file-exists-p (llemacs--pj-lock-path pj-id))
    (with-temp-buffer
      (insert-file-contents (llemacs--pj-lock-path pj-id))
      (buffer-string))))

(defun llemacs--pj-lock-check-stale (pj-id)
  "Check if lock for PJ-ID is stale (process/Emacs no longer running)."
  (when-let ((lock-info (llemacs--pj-lock-check pj-id)))
    (let* ((parts (split-string lock-info "@"))
           (user (car parts))
           (host (cadr parts)))
      (condition-case nil
          (with-temp-buffer
            (call-process "ssh" nil t nil host "pgrep" "-u" user "emacs")
            (= (buffer-size) 0))
        (llemacs--logging-write-error-pj t)))))

(defun llemacs--pj-lock-force-release (pj-id &optional force)
  "Force release lock for PJ-ID. Use with caution."
  (interactive "sProject ID to force unlock: ")
  (when (or force
            (not (called-interactively-p 'any))
            (yes-or-no-p (format "Really force unlock project %s? This should only be used in emergencies." pj-id)))
    (llemacs--pj-lock-release pj-id)
    (when (equal llemacs--cur-pj pj-id)
      (setq llemacs--cur-pj nil))))

(defun llemacs--pj-lock-cleanup ()
  "Clean up project lock on Emacs exit or crash."
  (condition-case err
      (when llemacs--cur-pj
        (llemacs--pj-lock-release llemacs--cur-pj))
    (llemacs--logging-write-error-pj (message "Failed to cleanup lock: %s" err))))

(defun llemacs--pj-lock-cleanup-all ()
  "Clean up all stale project locks."
  (dolist (lock-file (directory-files llemacs--path-projects t "\\.lock$"))
    (let ((pj-id (file-name-nondirectory (directory-file-name (file-name-directory lock-file)))))
      (when (llemacs--pj-lock-check-stale pj-id)
        (llemacs--pj-lock-release pj-id)))))

;; Add cleanup hooks and periodic check
(add-hook 'kill-emacs-hook #'llemacs--pj-lock-cleanup)
(run-with-timer 0 3600 #'llemacs--pj-lock-cleanup-all)
(add-hook 'after-init-hook #'llemacs--pj-lock-cleanup-all)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))