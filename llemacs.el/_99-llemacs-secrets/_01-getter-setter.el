;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-07 07:33:27
;;; Time-stamp: <2025-01-07 07:33:27 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-lemacs-secrets/01-getter-setter.el

(defun llemacs--secrets-set (key value)
  (let ((file (expand-file-name key llemacs--path-res-secrets)))
    (with-temp-file file
      (insert value))
    (set-file-modes file #o600)))

(defun llemacs--secrets-get (key)
  (let ((file (expand-file-name key llemacs--path-res-secrets)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))
