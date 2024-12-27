;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 19:29:49
;;; Time-stamp: <2024-12-27 19:29:49 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/elmo.el

(defvar elmo-lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing ELMO's Elisp files.")

(add-to-list 'load-path elmo-lisp-dir)

(require '01-elmo-config)
(require '02-elmo-logging-core)
(require '04-elmo-utils)
(require '06-elmo-json-md)
(require '07-elmo-exec)
(require '08-elmo-prompt-templates)
(require '09-elmo-lang2elisp)
(require '10-elmo-run)
(require '11-elmo-project-handler.el)
(require '11-elmo-context-handler.el)
(require '13-elmo-step.el)

(provide 'elmo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))