;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 18:55:04
;;; Time-stamp: <2025-01-03 18:55:04 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/paths_.el

;; Prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-prompt-templates
  (expand-file-name "resources/prompt-templates" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-prompt-compiled
  (expand-file-name "resources/prompt-templates/compiled" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs--path-prompt-components
  (expand-file-name "resources/prompt-templates/components" llemacs-path-workspace)
  "Directory for prompt components."
  :type 'file
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))