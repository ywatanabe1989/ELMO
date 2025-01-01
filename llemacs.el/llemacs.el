;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:57:43
;;; Time-stamp: <2025-01-01 22:57:43 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/llemacs.el

(defun llemacs--load-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-llemacs-base/00-entry.el" dir))
    (load (expand-file-name "02-llemacs-logging/00-entry.el" dir))
    (load (expand-file-name "03-llemacs-llm/00-entry.el" dir))
    (load (expand-file-name "04-llemacs-cvt/00-entry.el" dir))
    (load (expand-file-name "05-llemacs-run/00-entry.el" dir))
    (load (expand-file-name "06-llemacs-pm/00-entry.el" dir))
    ))

(llemacs--load-components)


(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))