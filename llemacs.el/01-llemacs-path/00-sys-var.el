;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:22:36
;;; Time-stamp: <2025-01-06 20:22:36 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/00-sys-var.el


;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 10:44:21
;;; Time-stamp: <2025-01-04 10:44:21 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/100-paths-sys.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path
  (expand-file-name "../../" (file-name-directory (or load-file-name buffer-file-name)))
  "Base directory for LLEMACS project."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-workspace
  (expand-file-name "workspace" llemacs--path)
  "Base directory for LLEMACS workspace."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-python-env-sys
  (expand-file-name ".env" llemacs--path-workspace)
  "Path to the Python environment used by llemacs.el."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-python-sys
  (expand-file-name "bin/python" llemacs--path-python-env-sys)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-sys
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-agents
  (expand-file-name "agents" llemacs--path-workspace)
  "Directory for LLEMACS agents."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-agent
  (expand-file-name (user-login-name) llemacs--path-agents)
  "User-specific home directory for LLEMACS."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-agent)

(defcustom llemacs--path-agent-emacs-server
  (expand-file-name ".emacs.d/emacs-server/server" llemacs--path-agent)
  "Path to LLEMACS Emacs server socket."
  :type 'file
  :group 'llemacs-path
  :group 'llemacs-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-res
  (expand-file-name "resources" llemacs--path-workspace)
  "Directory for LLEMACS resources."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defvar llemacs--path-res-secrets
  (expand-file-name ".secrets" llemacs--path-res)
  "Directory for storing sensitive information."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys
  :group 'llemacs-secret)

(defcustom llemacs--path-res-templates
  (expand-file-name "templates" llemacs--path-res)
  "Directory for LLEMACS templates."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompts
  (expand-file-name "prompts" llemacs--path-res)
  "Directory for LLEMACS templates."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-compiled
  (expand-file-name "compiled" llemacs--path-res-prompts)
  "Directory for compiled prompt templates."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-components
  (expand-file-name "components" llemacs--path-res-prompts)
  "Directory for prompt template components."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-recipes
  (expand-file-name "recipes" llemacs--path-res-prompts)
  "Directory for prompt template recipes."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-scripts
  (expand-file-name "scripts" llemacs--path-res)
  "Directory for LLEMACS scripts."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-tools
  (expand-file-name "tools" llemacs--path-res)
  "Directory for LLEMACS tools."
  :type 'directory
  :group 'llemacs-path
  :group 'llemacs-sys)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
