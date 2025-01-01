;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:57:29
;;; Time-stamp: <2025-01-01 22:57:29 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/06-llemacs-pm/00-entry.el

(defun llemacs--load-project-management-components ()
  "Load project-management component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "project.el" dir))
    (load (expand-file-name "step.el" dir))))

(llemacs--load-project-management-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))