;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 20:57:11
;;; Time-stamp: <2025-01-03 20:57:11 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/005-debug.el

(defcustom llemacs--is-debug-mode nil
  "When non-nil, enable debug logging and verbose output."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs--verbose nil
  "When non-nil, display additional operation details."
  :type 'boolean
  :group 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))