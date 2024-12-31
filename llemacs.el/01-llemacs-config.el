;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:42:05
;;; Time-stamp: <2024-12-31 22:42:05 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-config.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)

(defun llemacs--load-config-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-llemacs-config/01-llemacs-config-groups.el" dir))
    (load (expand-file-name "01-llemacs-config/01-llemacs-config-paths.el" dir))
    (load (expand-file-name "01-llemacs-config/01-llemacs-config-buffers.el" dir))
    (load (expand-file-name "01-llemacs-config/01-llemacs-config-debug.el" dir))
    (load (expand-file-name "01-llemacs-config/01-llemacs-config-timestamp.el" dir))))

;; Initialize components
(llemacs--load-config-components)

(provide '01-llemacs-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))