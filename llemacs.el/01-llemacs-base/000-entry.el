;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 11:10:30
;;; Time-stamp: <2025-01-04 11:10:30 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/000-entry.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)


(defun llemacs--load-base-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "000-list.el" dir))
    (load (expand-file-name "001-groups" dir))
    (load (expand-file-name "010-buf-var" dir))
    (load (expand-file-name "011-buf-func.el" dir))
    (load (expand-file-name "100-paths-sys" dir))
    (load (expand-file-name "100-paths-sys-log" dir))
    (load (expand-file-name "101-paths-pj" dir))
    (load (expand-file-name "101-paths-pj-log" dir))
    (load (expand-file-name "102-paths-pj-lock-system.el" dir))
    (load (expand-file-name "103-paths-pj-handlers.el" dir))
    (load (expand-file-name "999-debug.el" dir))
    (load (expand-file-name "999-md-json-loaders.el" dir))
    (load (expand-file-name "999-timestamp.el" dir))))

;; Initialize components
(llemacs--load-base-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))