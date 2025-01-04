;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 04:28:48
;;; Time-stamp: <2025-01-03 04:28:48 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-proj/00-entry.el

(defun llemacs--load-project-management-components ()
  "Load project-management component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "proj-init.el" dir))
    (load (expand-file-name "proj-variables.el" dir))
    (load (expand-file-name "proj-collect-context.el" dir))))

(llemacs--load-project-management-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))