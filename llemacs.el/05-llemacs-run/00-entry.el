;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-12 01:48:26
;;; Timestamp: <2025-01-12 01:48:26>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-run-components ()
  "Load run component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-run-elisp.el" dir))
    (load (expand-file-name "02-run-prompt.el" dir))
    (load (expand-file-name "03-run-progn-helpers.el" dir))
    (load (expand-file-name "03-run-progn.el" dir))
    (load (expand-file-name "04-run-project-management.el" dir))
    (load (expand-file-name "05-run-steps.el" dir))
    (load (expand-file-name "06-run-select.el" dir))
    (load (expand-file-name "06-run-search.el" dir))
    ))

(llemacs--load-run-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
