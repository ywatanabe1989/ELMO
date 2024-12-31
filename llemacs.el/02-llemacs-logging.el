;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:47:14
;;; Time-stamp: <2024-12-31 17:47:14 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging.el

(defun load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-db" dir))
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-file" dir))
    (load (expand-file-name "02-llemacs-logging/core/02-llemacs-logging-core-utils" dir))

    ;; Database components
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-getters" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-initializers" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-loggers" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-maintainers" dir))
    (load (expand-file-name "02-llemacs-logging/db/02-llemacs-logging-db-viewers" dir))

    ;; File components
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-getters" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-initializers" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-loggers" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-maintainers" dir))
    (load (expand-file-name "02-llemacs-logging/file/02-llemacs-logging-file-viewers" dir))))

;; Initialize components
(load-logging-components)

;; Initialize both logging systems
(llemacs--logging-init-db)
(llemacs--logging-init-file-system)
(provide '02-llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))