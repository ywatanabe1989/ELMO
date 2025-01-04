;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 01:49:41
;;; Time-stamp: <2025-01-04 01:49:41 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/viewers.el

;; (require '02-llemacs-logging-core-utils)
;; (require '02-llemacs-logging-file-getters)

;; (defun llemacs--logging-view-logs-by-level (level)
;;   "View logs of specified LEVEL in buffer."
;;   (interactive
;;    (list (completing-read "Log level: "
;;                           '("debug" "info" "warn" "error")
;;                           nil t)))
;;   (let ((entries (llemacs--logging-get-logs-by-level (intern level))))
;;     (llemacs--logging-display-buffer llemacs--buffer-logging t)
;;     (with-current-buffer llemacs--buffer-logging
;;       ;; (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (dolist (entry entries)
;;         (insert entry "\n")))))

(defun llemacs--logging-view-logs-by-level (level)
  "View logs of specified LEVEL in buffer."
  (interactive
   (list (completing-read "Log level: "
                          (mapcar #'symbol-name
                                  (seq-filter
                                   (lambda (l)
                                     (>= (llemacs--logging-get-level-value l)
                                         (llemacs--logging-get-level-value llemacs--logging-level-threshold)))
                                   (mapcar #'car llemacs--log-levels-sys)))
                          nil t)))
  (let ((entries (llemacs--logging-get-logs-by-level (intern level))))
    (llemacs--logging-display-buffer llemacs--buffer-logging t)
    (with-current-buffer llemacs--buffer-logging
      (erase-buffer)
      (dolist (entry entries)
        (insert entry "\n")))))

;; (llemacs--logging-view-logs-by-level "error")

(defun llemacs--logging-view-all-logs ()
  "View all logs in buffer."
  (interactive)
  (let ((entries (llemacs--logging-get-all-logs)))
    (llemacs--logging-display-buffer llemacs--buffer-logging t)
    (with-current-buffer llemacs--buffer-logging
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry entries)
          (insert entry "\n"))))))

;; (llemacs--logging-view-all-logs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))