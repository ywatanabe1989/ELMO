;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 05:58:23
;;; Timestamp: <2025-01-08 05:58:23>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-secrets/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; (defun llemacs--load-secrets-components ()
;;   "Load run component files."
;;   (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
;;     (load (expand-file-name "01-getter-setter.el" dir))
;;     (load (expand-file-name "01-git.el" dir))
;;     (load (expand-file-name "02-access-tokens.el" dir))
;;     (load (expand-file-name "03-ssh-keys.el" dir))
;;     ;; (load (expand-file-name "helpers.el" dir))
;;     ))

;; (llemacs--load-secrets-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
