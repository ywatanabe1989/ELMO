;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 22:57:59
;;; Timestamp: <2025-01-08 22:57:59>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/02-pj-var.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 00:38:12
;;; Time-stamp: <2025-01-05 00:38:12 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/101-paths-pj.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For all projects
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
;; For the currently pointed project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--cur-pj ""
  "Currently active project ID in the form of <id>-<project-name>."
  :type 'string
  :group 'llemacs-path
  :group 'llemacs-project)

;; (defcustom llemacs--path-pj
;;   (expand-file-name llemacs--cur-pj llemacs--path-projects)
;;   "Root directory for the current project."
;;   :type 'directory
;;   :group 'llemacs-path
;;   :group 'llemacs-project)


(defcustom llemacs--path-pj
  (when llemacs--cur-pj
    (expand-file-name llemacs--cur-pj llemacs--path-projects))
  "Root directory for the current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

;; Duplicated but since LLM often use this variable
(defcustom llemacs--path-pj-root
  (expand-file-name llemacs--cur-pj llemacs--path-projects)
  "Root directory for the current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-lock
  (expand-file-name ".lock" llemacs--path-pj)
  "Lock file for the current project."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-pm-mmd
  (expand-file-name "project_management/project_management.mmd" llemacs--path-pj)
  "Project management directory for the current project."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-config
  (expand-file-name "config" llemacs--path-pj)
  "Configuration directory for the current project configuration."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-data
  (expand-file-name "data" llemacs--path-pj)
  "Data directory for the current project data."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-scripts
  (expand-file-name "scripts" llemacs--path-pj)
  "Scripts directory for the current project scripts."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-results
  (expand-file-name "results" llemacs--path-pj)
  "Resutls directory for the current project results."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-results-figures
  (expand-file-name "figures" llemacs--path-pj-results)
  "Resutls directory for the current project results."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-reports
  (expand-file-name "reports" llemacs--path-pj)
  "Reports directory for the current project results."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-python-env
  (expand-file-name ".env" llemacs--path-pj)
  "Python environment path for the current project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-python
  (expand-file-name "bin/python" llemacs--path-pj-python-env)
  "Python binary path for the current project."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-project)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
