;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:36:42
;;; Time-stamp: <2025-01-06 17:36:42 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-proj/00-entry.el
;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-project-management-components ()
  "Load project-management component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-variables.el" dir))
    (load (expand-file-name "02-init.el" dir))
    (load (expand-file-name "03-lock.el" dir))
    (load (expand-file-name "04-collect-context.el" dir))
    (load (expand-file-name "05-mermaid-save-hook.el" dir))))

(llemacs--load-project-management-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
