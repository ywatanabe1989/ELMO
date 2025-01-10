;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:26:20
;;; Timestamp: <2025-01-11 08:26:20>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/05-pj-switch.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--pj-set-cur-pj (pj-id &optional force)
  "Set PJ-ID as the current active project."
  (llemacs--pj-lock-force-release pj-id force)
  (if-let ((err-msg (llemacs--validate-pj-id pj-id)))
      (llemacs--logging-write-error-pj err-msg)
    (when-let ((lock-info (llemacs--pj-lock-check pj-id)))
      (llemacs--logging-write-error-pj "Project is locked by %s. Only one user/process can switch to and work on a project at a time." lock-info))
    (setq llemacs--cur-pj pj-id)
    (llemacs--path-pj-update)
    (llemacs--pj-set-last-pj)
    (llemacs--pj-lock-acquire pj-id)
    ))

;; (llemacs--pj-update-symlink "103-epilepsty-seizure-detection")

(defun llemacs--pj-set-last-pj ()
  "Save current project ID to file."
  (when llemacs--cur-pj
    (unless (file-exists-p llemacs--path-projects)
      (make-directory llemacs--path-projects t))
    (with-temp-file (expand-file-name ".last-project" llemacs--path-projects)
      (insert llemacs--cur-pj))))

(defun llemacs--validate-pj-id (pj-id)
  "Validate PJ-ID format.
Returns error message if format is invalid, nil otherwise."
  (when (and (not (string-empty-p pj-id))
             (not (string-match-p "^[0-9]+-[a-zA-Z0-9-]+$" pj-id)))
    (format "Invalid project ID format. Expected '<id>-<project-name>' but got '%s'" pj-id)))

(defun llemacs--pj-auto-set ()
  "Auto-set project based on current directory."
  (let* ((current-path (file-truename default-directory))
         (projects (llemacs--pj-get-available-pjs))
         (matching-project
          (seq-find
           (lambda (proj)
             (string-prefix-p
              (file-truename (expand-file-name proj llemacs--path-projects))
              current-path))
           projects)))
    (when matching-project
      (llemacs--pj-switch matching-project))))

;; Add to startup hook
(add-hook 'after-init-hook #'llemacs--pj-auto-set)

(defun llemacs--pj-init-default ()
  "Initialize default project at startup."
  (when (or (not llemacs--cur-pj) (string-empty-p llemacs--cur-pj))
    (when-let ((default-pj (llemacs--pj-get-default-pj)))
      (llemacs--pj-switch default-pj))))

(add-hook 'after-init-hook #'llemacs--pj-init-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--pj-get-available-pjs ()
  "Get list of available projects."
  (when (file-directory-p llemacs--path-projects)
    (seq-filter
     (lambda (dir)
       (and (file-directory-p (expand-file-name dir llemacs--path-projects))
            (string-match-p "^[0-9]+-[a-zA-Z0-9-]+$" dir)))
     (directory-files llemacs--path-projects))))

(defun llemacs--pj-get-cur-pj ()
  "Get currently active project ID."
  llemacs--cur-pj)

(defun llemacs--pj-get-last-pj ()
  "Get last used project ID from file."
  (let ((last-project-file (expand-file-name ".last-project" llemacs--path-projects)))
    (when (file-exists-p last-project-file)
      (with-temp-buffer
        (insert-file-contents last-project-file)
        (let ((proj (buffer-string)))
          (and (member proj (llemacs--pj-get-available-pjs))
               proj))))))

(defun llemacs--pj-get-latest-pj ()
  "Get the project with the highest ID number."
  (car (sort (llemacs--pj-get-available-pjs) #'string>)))

(defun llemacs--pj-get-default-pj ()
  "Get default project (last used or latest)."
  (or (llemacs--pj-get-last-pj)
      (llemacs--pj-get-latest-pj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'llemacs--pj-switch
  'llemacs--pj-set-cur-pj
  "Set PJ-ID as the current active project.")

(defun llemacs-pj-switch (pj-id &optional force)
  "Interactive and programmatic interface to switch to project PJ-ID."
  (interactive
   (list (completing-read (format "Switch from %s to: " (llemacs--pj-get-cur-pj))
                          (llemacs--pj-get-available-pjs)
                          nil t nil nil
                          (llemacs--pj-get-default-pj))))
  (when pj-id
    (let ((msg (format "Switched to project: %s" pj-id)))
      (llemacs--pj-switch pj-id force)
      (llemacs--pj-set-last-pj)
      (llemacs--logging-write-success-sys msg)
      (message msg))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
