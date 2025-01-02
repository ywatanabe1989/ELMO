;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 00:40:39
;;; Time-stamp: <2025-01-03 00:40:39 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/db-initializers.el

(require 'emacsql)
(require 'emacsql-sqlite)


(defun llemacs--logging-db-init-if (&optional project-id)
  "Initialize database for structured logging."
  (let ((db-path (if project-id
                     (expand-file-name "logs/logging.db"
                                       (expand-file-name (format "%s-" project-id)
                                                         llemacs-path-projects))
                   llemacs--path-logging-db)))
    (unless (file-exists-p (file-name-directory db-path))
      (make-directory (file-name-directory db-path) t))
    (setq llemacs--logging-db-connection (emacsql-sqlite-open db-path))
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
    (message "Log database initialized at %s" db-path)))

(llemacs--logging-db-init-if "000")

;; (defun llemacs--logging-db-init-if (&optional project-id)
;;   "Initialize database for structured logging."
;;   (unless (file-exists-p (file-name-directory llemacs--path-logging-db))
;;     (make-directory (file-name-directory llemacs--path-logging-db) t))
;;   (setq llemacs--logging-db-connection (emacsql-sqlite-open llemacs--path-logging-db))
;;   (dolist (table-spec llemacs--logging-db-schema)
;;     (let* ((table-name (car table-spec))
;;            (columns (cadr table-spec)))
;;       (emacsql llemacs--logging-db-connection
;;                [:create-table-if-not-exists $i1 $v2]
;;                table-name
;;                (vconcat (mapcar (lambda (col)
;;                                   (vector (intern (replace-regexp-in-string "-" "_" (symbol-name (car col))))
;;                                           (intern (format ":%s" (cadr col)))))
;;                                 columns)))))
;;   (message "Log database initialized at %s" llemacs--path-logging-db))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))