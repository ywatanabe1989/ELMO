;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:15:14
;;; Time-stamp: <2024-12-31 23:15:14 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)

(defun llemacs--load-base-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-llemacs-base/01-llemacs-base-groups.el" dir))
    (load (expand-file-name "01-llemacs-base/01-llemacs-base-paths.el" dir))
    (load (expand-file-name "01-llemacs-base/01-llemacs-base-buffers.el" dir))
    (load (expand-file-name "01-llemacs-base/01-llemacs-base-debug.el" dir))
    (load (expand-file-name "01-llemacs-base/01-llemacs-base-timestamp.el" dir))))

;; Initialize components
(llemacs--load-base-components)

(provide '01-llemacs-base)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))