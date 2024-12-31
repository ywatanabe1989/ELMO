;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:51:31
;;; Time-stamp: <2024-12-31 22:51:31 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/db/02-llemacs-logging-db-initializers.el

(require '01-llemacs-config)
(require '02-llemacs-logging-core-db)
(require 'emacsql)
(require 'emacsql-sqlite)

(defun llemacs--logging-init-db ()
  "Initialize database for structured logging."
  (unless (file-exists-p (file-name-directory llemacs--path-logging-db))
    (make-directory (file-name-directory llemacs--path-logging-db) t))
  (setq llemacs--logging-db-connection (emacsql-sqlite-open llemacs--path-logging-db))
  (dolist (table-spec llemacs--logging-db-schema)
    (let* ((table-name (car table-spec))
           (columns (cadr table-spec)))
      (emacsql llemacs--logging-db-connection
               [:create-table-if-not-exists $i1 $v2]
               table-name
               (vconcat (mapcar (lambda (col)
                                  (vector (intern (replace-regexp-in-string "-" "_" (symbol-name (car col))))
                                          (intern (format ":%s" (cadr col)))))
                                columns)))))
  (message "Log database initialized at %s" llemacs--path-logging-db))


(provide '02-llemacs-logging-db-initializers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))