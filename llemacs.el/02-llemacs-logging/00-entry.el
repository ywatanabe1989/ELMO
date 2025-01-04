;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 01:51:04
;;; Time-stamp: <2025-01-04 01:51:04 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/00-entry.el

(defun llemacs--load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "variables.el" dir))

    ;; File components
    (load (expand-file-name "getters.el" dir))
    (load (expand-file-name "initializers.el" dir))
    (load (expand-file-name "level.el" dir))
    (load (expand-file-name "loggers.el" dir))
    (load (expand-file-name "maintainers.el" dir))
    (load (expand-file-name "viewers.el" dir))))

;; Initialize components
(llemacs--load-logging-components)

;; Initialize both logging systems
;; (llemacs--logging-db-init-if)
(llemacs--logging-system-files-init-if)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))