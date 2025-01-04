;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 00:38:12
;;; Time-stamp: <2025-01-05 00:38:12 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/101-paths-pj.el

;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-projects
  (expand-file-name "projects" llemacs--path-workspace)
  "Directory for LLEMACS projects."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-sample-project-zip
  (expand-file-name "000-sample-project.zip" llemacs--path-projects)
  "Sample project zip file for initializing project structure."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--cur-pj "000-test"
  "Currently active project ID in the form of <id>-<project-name>."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-pj
  (expand-file-name llemacs--cur-pj llemacs--path-projects)
  "Directory for current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

;; Duplicated but since LLM often use this variable
(defcustom llemacs--path-pj-root
  (expand-file-name llemacs--cur-pj llemacs--path-projects)
  "Directory for current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-lock
  (expand-file-name ".lock" llemacs--path-pj)
  "Lock file for current project."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-config
  (expand-file-name "config" llemacs--path-pj)
  "Directory for current project configuration."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-data
  (expand-file-name "data" llemacs--path-pj)
  "Directory for current project data."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-results
  (expand-file-name "results" llemacs--path-pj)
  "Directory for current project results."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-scripts
  (expand-file-name "scripts" llemacs--path-pj)
  "Directory for current project scripts."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-pm-mmd
  (expand-file-name "project_management/project_management.mmd" llemacs--path-pj)
  "Directory for current project management."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-python-env
  (expand-file-name ".env" llemacs--path-pj)
  "Python environment path for current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--path-pj-python
  (expand-file-name "bin/python" llemacs--path-pj-python-env)
  "Python path for current project."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-script
  :group 'llemacs-sys)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))