;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 03:00:34
;;; Time-stamp: <2025-01-02 03:00:34 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/00-entry.el

(defun llemacs--load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "db-variables.el" dir))
    (load (expand-file-name "file-variables.el" dir))

    ;; Database components
    (load (expand-file-name "db-getters.el" dir))
    (load (expand-file-name "db-initializers.el" dir))
    (load (expand-file-name "db-loggers.el" dir))
    (load (expand-file-name "db-maintainers.el" dir))
    (load (expand-file-name "db-viewers.el" dir))

    ;; File components
    (load (expand-file-name "file-getters.el" dir))
    (load (expand-file-name "file-initializers.el" dir))
    (load (expand-file-name "file-loggers.el" dir))
    (load (expand-file-name "file-maintainers.el" dir))
    (load (expand-file-name "file-viewers.el" dir))))

;; Initialize components
(llemacs--load-logging-components)

;; Initialize both logging systems
(llemacs--logging-db-init-if)
(llemacs--logging-system-files-init-if)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))