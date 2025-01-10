;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:46:45
;;; Timestamp: <2025-01-11 08:46:45>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'custom)
(require 'json)

(defun llemacs--load-base-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "00-sys-var.el" dir))
    (load (expand-file-name "01-sys-log.el" dir))
    (load (expand-file-name "02-pj-var.el" dir))
    (load (expand-file-name "03-pj-log.el" dir))
    (load (expand-file-name "04-pj-updater.el" dir))
    (load (expand-file-name "05-pj-symlink.el" dir))
    ))

;; Initialize components
(llemacs--load-base-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
