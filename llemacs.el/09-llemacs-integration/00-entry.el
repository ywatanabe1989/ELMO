;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 21:50:58
;;; Timestamp: <2025-01-08 21:50:58>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-integration-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; (load (expand-file-name "00-integration.el" dir))
    (load (expand-file-name "01-elisp.el" dir))
    (load (expand-file-name "02-find-bin.el" dir))
    (load (expand-file-name "03-shell.el" dir))
    (load (expand-file-name "04-git.el" dir))
    (load (expand-file-name "05-github.el" dir))
    ))

;; Initialize components
(llemacs--load-integration-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
