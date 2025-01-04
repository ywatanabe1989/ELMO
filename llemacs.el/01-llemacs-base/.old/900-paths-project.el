;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 22:41:06
;;; Time-stamp: <2025-01-03 22:41:06 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/900-paths-project.el

;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-project-logs nil
  "Base path for project logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-project-logs-by-level nil
  "Base path for project logs by level."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-project-all nil
  "Base path for all project logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-projects
  (expand-file-name "projects" llemacs-path-workspace)
  "Directory for projects."
  :type 'file
  :group 'llemacs-project)

(defcustom llemacs--path-sample-project-zip
  (expand-file-name "000-sample-project.zip" llemacs--path-projects)
  "Sample project zip file for initializing project structure."
  :type 'file
  :group 'llemacs-project)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))