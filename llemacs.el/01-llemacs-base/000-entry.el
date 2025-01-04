;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:48:28
;;; Time-stamp: <2025-01-04 14:48:28 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/000-entry.el

(require 'custom)
(require 'json)
(require 'emacsql)
(require 'emacsql-sqlite)


(defun llemacs--load-base-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "000-list.el" dir))
    (load (expand-file-name "001-groups.el" dir))
    (load (expand-file-name "010-buf-var.el" dir))
    (load (expand-file-name "011-buf-func.el" dir))
    (load (expand-file-name "100-paths-sys.el" dir))
    (load (expand-file-name "100-paths-sys-log.el" dir))
    (load (expand-file-name "101-paths-pj.el" dir))
    (load (expand-file-name "101-paths-pj-log.el" dir))
    (load (expand-file-name "102-paths-pj-lock-system.el" dir))
    (load (expand-file-name "103-paths-pj-handlers.el" dir))
    (load (expand-file-name "999-debug.el" dir))
    (load (expand-file-name "999-md-json-loaders.el" dir))
    (load (expand-file-name "999-timestamp.el" dir))))

;; Initialize components
(llemacs--load-base-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))