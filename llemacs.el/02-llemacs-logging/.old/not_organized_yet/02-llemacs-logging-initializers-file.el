;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:59:36
;;; Time-stamp: <2024-12-31 16:59:36 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-initializers-file.el

;; Just placeholder for consistency

(provide '02-llemacs-logging-file)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))