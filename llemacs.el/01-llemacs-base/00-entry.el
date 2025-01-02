;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:49:11
;;; Time-stamp: <2025-01-02 10:49:11 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/00-entry.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)

(defun llemacs--load-base-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "groups.el" dir))
    (load (expand-file-name "paths.el" dir))
    (load (expand-file-name "buffers.el" dir))
    (load (expand-file-name "debug.el" dir))
    (load (expand-file-name "timestamp.el" dir))))

;; Initialize components
(llemacs--load-base-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))