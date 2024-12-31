;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:11:15
;;; Time-stamp: <2024-12-31 17:11:15 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/file/viewers.el

(require '02-llemacs-logging-core-utils)
(require '02-llemacs-logging-file-getters)

(defun llemacs--logging-view-logs-by-level (level)
  "View logs of specified LEVEL in buffer."
  (interactive
   (list (completing-read "Log level: "
                          '("debug" "info" "warn" "error")
                          nil t)))
  (let* ((entries (llemacs--logging-get-logs-by-level (intern level)))
         (buf (get-buffer-create (format "*LLEMACS %s Logs*" (upcase level)))))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (entry entries)
        (insert entry "\n")))
    (llemacs--logging-display-buffer buf)))

(defun llemacs--logging-view-all-logs ()
  "View all logs in buffer."
  (interactive)
  (let* ((entries (llemacs--logging-get-all-logs))
         (buf (get-buffer-create "*LLEMACS All Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (entry entries)
        (insert entry "\n")))
    (llemacs--logging-display-buffer buf)))

(provide '02-llemacs-logging-file-viewers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))