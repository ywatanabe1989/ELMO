;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 02:55:12
;;; Time-stamp: <2025-01-03 02:55:12 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-project/00-entry.el

(defun llemacs--load-project-management-components ()
  "Load project-management component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "project_variables.el" dir))
    (load (expand-file-name "project_init.el" dir))
    (load (expand-file-name "project_collect-contest.el" dir))))

(llemacs--load-project-management-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))