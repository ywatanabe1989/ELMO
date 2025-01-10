;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:43:04
;;; Timestamp: <2025-01-11 08:43:04>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/04-pj-lock-system.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lock System; Only one user/process can switch to and work on a project at a time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
