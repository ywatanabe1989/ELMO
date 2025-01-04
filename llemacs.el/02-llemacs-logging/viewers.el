;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:50:57
;;; Time-stamp: <2025-01-04 12:50:57 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/viewers.el

(defun llemacs--logging-view (&optional select-level is-pj)
  "Display logs in buffer.
When SELECT-LEVEL is non-nil, prompt for specific log level.
When IS-PJ is non-nil, show project logs instead of system logs."
  (interactive
   (list (y-or-n-p "Select specific log level? ")
         (y-or-n-p "View project logs? ")))
  (let* ((level (when select-level
                  (completing-read "Log level: "
                                   (mapcar #'symbol-name
                                           (seq-filter
                                            (lambda (l)
                                              (>= (llemacs--logging-get-level-value l)
                                                  (llemacs--logging-get-level-value llemacs--logging-level-threshold)))
                                            (mapcar #'car llemacs--log-levels-sys)))
                                   nil t)))
         (entries (cond
                   ((and level is-pj) (llemacs--logging-get-pj-logs-by-level (intern level)))
                   (level (llemacs--logging-get-logs-by-level (intern level)))
                   (is-pj (llemacs--logging-get-all-pj-logs))
                   (t (llemacs--logging-get-all-logs)))))
    (if is-pj
        (llemacs--buf-disp-logging-pj nil t t)
      (llemacs--buf-disp-logging-sys nil t t))
    (with-current-buffer (if is-pj llemacs--buf-logging-pj llemacs--buf-logging-sys)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry entries)
          (insert entry "\n"))))))

;; (llemacs--logging-view)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))