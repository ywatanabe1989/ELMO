;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 20:17:35
;;; Time-stamp: <2024-12-31 20:17:35 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging.el

(defun load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-db.el" dir))
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-file.el" dir))
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-utils.el" dir))

    ;; Database components
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-getters.el" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-initializers.el" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-loggers.el" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-maintainers.el" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-viewers.el" dir))

    ;; File components
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-getters.el" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-initializers.el" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-loggers.el" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-maintainers.el" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-viewers.el" dir))))

;; Initialize components
(load-logging-components)

;; Initialize both logging systems
(llemacs--logging-init-db)
(llemacs--logging-init-file-system)
(provide '02-llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))