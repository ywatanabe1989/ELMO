;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 04:33:31
;;; Timestamp: <2025-01-09 04:33:31>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-project/02-init.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--pj-get-next-id ()
  "Get next available project ID."
  (let ((id 1))
    (when (file-exists-p llemacs--path-latest-project-id)
      (with-temp-buffer
        (insert-file-contents llemacs--path-latest-project-id)
        (setq id (1+ (string-to-number (buffer-string))))))
    (with-temp-file llemacs--path-latest-project-id
      (insert (number-to-string id)))
    (format "%03d" id)))

(defun llemacs--pj-get-dir (full-pj-name)
  "Get project directory for FULL-PJ-NAME."
  (unless full-pj-name
    (error "Project ID/name cannot be nil")
    (return-from llemacs--pj-get-dir nil))
  (unless (file-exists-p llemacs--path-projects)
    (error "Projects directory does not exist")
    (return-from llemacs--pj-get-dir nil))
  (unless (string-match-p "-" full-pj-name)
    (error "Project ID must be in format <ID>-<name>")
    (return-from llemacs--pj-get-dir nil))
  (let ((pattern (format "^%s$" full-pj-name)))
    (if-let ((dirname (car (directory-files llemacs--path-projects nil pattern))))
        (progn
          (expand-file-name dirname llemacs--path-projects))
      (error
       (format "No project found for <ID>-<name>: %s" full-pj-name))
      nil)))

(defun llemacs--pj-init-pm-mmd (full-pj-name pj-goals)
  "Initialize a mermaid file for project management."
  (with-temp-buffer
    (insert-file-contents llemacs--path-pj-pm-mmd)
    (goto-char (point-min))
    (while (search-forward "PJNAME[Project Name]" nil t)
      (replace-match (format "PJNAME[%s]" full-pj-name)))
    (goto-char (point-min))
    (while (search-forward "PJGOALS[Goals]" nil t)
      (replace-match (format "PJGOALS[%s]" pj-goals)))
    (goto-char (point-min))
    (while (search-forward "\\/workspace\\/projects\\/000-sample-project" nil t)
      (replace-match llemacs--path-pj))
    (write-region (point-min) (point-max) llemacs--path-pj-pm-mmd)))

(defun llemacs--pj-init (pj-name &optional goals)
  "Create new project with basic structure."
  (interactive "sProject Name: ")
  (let* ((project-id (llemacs--pj-get-next-id))
         (full-pj-name (format "%s-%s" project-id pj-name))
         (pj-goals (or goals (read-string "Project Goals: "))))
    (llemacs--pj-set-cur-pj full-pj-name)
    (if (file-exists-p llemacs--path-sample-project-zip)
        (progn
          (let ((default-directory llemacs--path-projects))
            (call-process "unzip" nil nil nil llemacs--path-sample-project-zip)
            (if (file-exists-p full-pj-name)
                (call-process "rsync" nil nil nil "-av" "--delete"
                              "000-sample-project/" (concat full-pj-name "/"))
              (rename-file "000-sample-project" full-pj-name))
            (llemacs--logging-write-success-pj
             (format "Project initialized: %s\nProject directory: %s"
                     full-pj-name
                     llemacs--path-pj))
            (llemacs--pj-init-pm-mmd full-pj-name pj-goals))
          project-id)
      (error "Project template not found")
      nil)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
