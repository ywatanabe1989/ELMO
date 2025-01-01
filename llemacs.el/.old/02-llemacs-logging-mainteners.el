;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:58:23
;;; Time-stamp: <2024-12-31 16:58:23 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-mainteners.el

;; ;; ;; (defun llemacs--logging-clear-old-entries (days)
;; ;; ;;   "Clear log entries older than DAYS."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (let ((cutoff (time-subtract (current-time)
;; ;; ;;                                (days-to-time days))))
;; ;; ;;     (emacsql llemacs--logging-db-connection
;; ;; ;;              [:delete from activities
;; ;; ;;                       where timestamp < $s1]
;; ;; ;;              (format-time-string "%Y-%m-%d %H:%M:%S" cutoff))))

;; ;; ;; (defun llemacs--logging-export-to-csv (table file)
;; ;; ;;   "Export TABLE to CSV FILE."
;; ;; ;;   (unless llemacs--logging-db-connection (llemacs--logging-init-db))
;; ;; ;;   (let ((data (emacsql llemacs--logging-db-connection
;; ;; ;;                        [:select * from $i1]
;; ;; ;;                        table)))
;; ;; ;;     (with-temp-file file
;; ;; ;;       (insert (mapconcat #'prin1-to-string (car data) ",") "\n")
;; ;; ;;       (dolist (row (cdr data))
;; ;; ;;         (insert (mapconcat #'prin1-to-string row ",") "\n")))))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))