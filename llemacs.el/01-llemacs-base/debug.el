;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:49:59
;;; Time-stamp: <2025-01-02 10:49:59 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/debug.el

(defcustom llemacs-debug nil
  "When non-nil, enable debug logging and verbose output."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs-verbose nil
  "When non-nil, display additional operation details."
  :type 'boolean
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))