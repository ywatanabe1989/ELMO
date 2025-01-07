;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:38:14
;;; Time-stamp: <2025-01-06 17:38:14 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/mdjson.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--cvt-format-script
  (expand-file-name "bin/mdjson" llemacs--path-python-env-sys)
  "Path to JSON/Markdown conversion script."
  :type 'file
  :group 'llemacs)

(defun llemacs--cvt-mdjson (md-or-json-path)
  "Convert MD/JSON file to JSON/MD using external script."
  (interactive
   (list (let ((default (and buffer-file-name buffer-file-name)))
           (read-file-name "File: " nil default t default))))
  (condition-case err
      (call-process llemacs--cvt-format-script nil nil nil (expand-file-name md-or-json-path))
    (error
     (llemacs--logging-write-error-sys (format "Conversion failed: %s" err))
     0)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
