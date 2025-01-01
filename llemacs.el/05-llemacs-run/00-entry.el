;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 03:13:31
;;; Time-stamp: <2025-01-02 03:13:31 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/05-llemacs-run/00-entry.el

(defun llemacs--load-run-components ()
  "Load run component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "run.el" dir))
    (load (expand-file-name "exec.el" dir))
    (load (expand-file-name "helpers.el" dir))))

(llemacs--load-run-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))