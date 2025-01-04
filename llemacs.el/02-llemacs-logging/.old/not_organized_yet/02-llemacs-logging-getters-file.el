;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:57:30
;;; Time-stamp: <2024-12-31 16:57:30 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-getters-file.el


(require '02-llemacs-logging-db)

(defun llemacs--logging-get-file-logs (&optional file)
  "Get contents of log FILE or system log."
  (let ((log-file (or file llemacs--path-log-system)))
    (when (file-exists-p log-file)
      (with-temp-buffer
        (insert-file-contents log-file)
        (buffer-string)))))

(defun llemacs--logging-get-recent-logs (&optional lines file)
  "Get LINES recent lines from FILE or system log."
  (let* ((log-file (or file llemacs--path-log-system))
         (num-lines (or lines 50)))
    (when (file-exists-p log-file)
      (with-temp-buffer
        (insert-file-contents log-file)
        (goto-char (point-max))
        (forward-line (- num-lines))
        (buffer-substring (point) (point-max))))))

(provide '02-llemacs-logging-getters-file)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))