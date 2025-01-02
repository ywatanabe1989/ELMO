;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 02:11:11
;;; Time-stamp: <2025-01-03 02:11:11 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-project/project_variables.el

(defcustom llemacs--project-splitter "\n----------------------------------------\n"
  "Splitter between logs."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs-project-default-name "default"
  "Default project name when none specified."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs-project-auto-save t
  "Whether to automatically save project state."
  :type 'boolean
  :group 'llemacs-project)

(defcustom llemacs-project-max-history 50
  "Maximum number of project history entries to keep."
  :type 'integer
  :group 'llemacs-project)

(defcustom llemacs--project-status-options
  '("planning" "in-progress" "review" "completed" "archived")
  "Available status options for LLEMACS projects."
  :type '(repeat string)
  :group 'llemacs)

(defcustom llemacs--path-project-id
  (expand-file-name ".project-id" llemacs-path-projects)
  "File to store the latest project ID."
  :type 'string
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))