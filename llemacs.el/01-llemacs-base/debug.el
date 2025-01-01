;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:06:28
;;; Time-stamp: <2024-12-31 23:06:28 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base/01-llemacs-base-debug.el

(defcustom llemacs-debug nil
  "When non-nil, enable debug logging and verbose output."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs-verbose nil
  "When non-nil, display additional operation details."
  :type 'boolean
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))