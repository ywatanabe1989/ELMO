;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:38:52
;;; Time-stamp: <2025-01-06 17:38:52 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/t2t-compress.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defvar llemacs--cvt-compress-max-length 1024
  "Maximum length for compressed text output.")

(defun llemacs--cvt-compress-text (text &optional max-len)
  "Compress TEXT using LLM within MAX-LEN characters."
  (let ((limit (or max-len llemacs--cvt-compress-max-length)))
    (if (<= (length text) limit)
        text
      (llemacs-run
       (format "Compress to %d chars, preserve key information:\n%s"
               limit text)))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
