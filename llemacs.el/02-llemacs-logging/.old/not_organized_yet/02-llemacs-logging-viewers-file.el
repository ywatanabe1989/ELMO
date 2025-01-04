;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:56:14
;;; Time-stamp: <2024-12-31 16:56:14 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-viewers-file.el

(require '02-llemacs-logging-db)

(defun llemacs--logging-display-buffer (buffer)
  "Display BUFFER in a dedicated window."
  (let ((window (split-window-below -10)))
    (set-window-buffer window buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (recenter -1))
    (select-window window)))

(defun llemacs--logging-view-file-logs ()
  "View contents of log file."
  (interactive)
  (let ((buf (find-file-noselect llemacs--path-log-system)))
    (llemacs--logging-display-buffer buf)))

(provide '02-llemacs-logging-viewers-file)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))