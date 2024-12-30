;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-30 17:21:35
;;; Time-stamp: <2024-12-30 17:21:35 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/llemacs.el

(defvar llemacs-lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing LLEMACS's Elisp files.")

(add-to-list 'load-path llemacs-lisp-dir)

(require '01-llemacs-config)
(require '02-llemacs-logging)
(require '04-llemacs-utils)
(require '06-llemacs-json-md)
(require '07-llemacs-exec)
(require '08-llemacs-prompt)
(require '08-llemacs-prompt-recipes)
(require '09-llemacs-lang2elisp)
(require '10-llemacs-run)
(require '11-llemacs-project)
(require '12-llemacs-context)
(require '13-llemacs-step)
(require '14-llemacs-search)

(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))