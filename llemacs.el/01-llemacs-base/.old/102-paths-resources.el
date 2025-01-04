;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 18:54:48
;;; Time-stamp: <2025-01-03 18:54:48 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/paths_.el

;; Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-resources
  (expand-file-name "resources" llemacs-path-workspace)
  "Directory for resources."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-templates
  (expand-file-name "resources/templates" llemacs-path-workspace)
  "Directory for templates."
  :type 'string
  :group 'llemacs)

(defcustom llemacs-path-agents
  (expand-file-name "resources/agents" llemacs-path-workspace)
  "Directory for agent definitions."
  :type 'string
  :group 'llemacs-api)

(defcustom llemacs-path-tools
  (expand-file-name "resources/tools" llemacs-path-workspace)
  "Directory for tool definitions."
  :type 'string
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))