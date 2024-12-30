;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:11:27
;;; Time-stamp: <2024-12-29 08:11:27 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/llemacs.el

(defvar llemacs-lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing ELMO's Elisp files.")

(add-to-list 'load-path llemacs-lisp-dir)

(require '01-llemacs-config)
(require '02-llemacs-logging-core)
(require '04-llemacs-utils)
(require '06-llemacs-json-md)
(require '07-llemacs-exec)
(require '08-llemacs-prompt)
(require '09-llemacs-lang2elisp)
(require '10-llemacs-run)
(require '11-llemacs-project-handler.el)
(require '11-llemacs-context-handler.el)
(require '13-llemacs-step.el)

(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))