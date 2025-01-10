;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 11:03:44
;;; Timestamp: <2025-01-10 11:03:44>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/00-entry.el

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
    (load (expand-file-name "00-list.el" dir))
    (load (expand-file-name "01-groups.el" dir))
    (load (expand-file-name "02-buf-var.el" dir))
    (load (expand-file-name "03-buf-func.el" dir))
    (load (expand-file-name "04-timestamp.el" dir))
    (load (expand-file-name "05-script-header.el" dir))
    (load (expand-file-name "06-md-json-loaders.el" dir))
    (load (expand-file-name "07-documentation.el" dir))
    ))

;; Initialize components
(llemacs--load-base-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
