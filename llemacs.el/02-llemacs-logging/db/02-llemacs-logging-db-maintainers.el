;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:08:29
;;; Time-stamp: <2024-12-31 17:08:29 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/db/maintainers.el

(require '02-llemacs-logging-core-db)

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

(provide '02-llemacs-logging-db-maintainers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))