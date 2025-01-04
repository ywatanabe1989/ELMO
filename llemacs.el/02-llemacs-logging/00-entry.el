;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:52:08
;;; Time-stamp: <2025-01-04 12:52:08 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/00-entry.el

(defun llemacs--load-logging-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "variables.el" dir))

    ;; File components
    (load (expand-file-name "getters.el" dir))
    ;; (load (expand-file-name "initializers.el" dir))
    (load (expand-file-name "level.el" dir))
    (load (expand-file-name "loggers.el" dir))
    ;; (load (expand-file-name "maintainers.el" dir))
    (load (expand-file-name "viewers.el" dir))))

;; Initialize components
(llemacs--load-logging-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))