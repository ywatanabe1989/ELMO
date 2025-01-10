;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:37:30
;;; Timestamp: <2025-01-11 08:37:30>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-variables.el" dir))
    (load (expand-file-name "02-get.el" dir))
    (load (expand-file-name "03-level.el" dir))
    (load (expand-file-name "04-write.el" dir))
    (load (expand-file-name "05-view.el" dir))))

;; Initialize components
(llemacs--load-logging-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
