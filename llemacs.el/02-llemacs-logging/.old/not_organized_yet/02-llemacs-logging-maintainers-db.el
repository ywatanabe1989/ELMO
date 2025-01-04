;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:01:03
;;; Time-stamp: <2024-12-31 17:01:03 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-maintainers-db.el

(require '02-llemacs-logging-db)

(defun llemacs--logging-clear-old-entries (days)
  "Clear log entries older than DAYS from database."
  (unless llemacs--logging-db-connection (llemacs--logging-init-db))
  (let ((cutoff (format-time-string "%Y-%m-%d %H:%M:%S"
                                    (time-subtract (current-time)
                                                   (days-to-time days)))))
    (dolist (table '(projects milestones tasks steps))
      (emacsql llemacs--logging-db-connection
               [:delete :from $i1
                        :where (< timestamp $s2)]
               table
               cutoff))))

(defun llemacs--logging-export-table-to-csv (table file)
  "Export TABLE from database to CSV FILE."
  (unless llemacs--logging-db-connection (llemacs--logging-init-db))
  (let ((data (emacsql llemacs--logging-db-connection
                       [:select * :from $i1]
                       table)))
    (with-temp-file file
      (insert (mapconcat #'prin1-to-string (car data) ",") "\n")
      (dolist (row (cdr data))
        (insert (mapconcat #'prin1-to-string row ",") "\n")))))

(defun llemacs--logging-vacuum-db ()
  "Compact the database by removing unused space."
  (unless llemacs--logging-db-connection (llemacs--logging-init-db))
  (emacsql llemacs--logging-db-connection [:vacuum]))

(provide '02-llemacs-logging-maintainers-db)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))