;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:27:18
;;; Time-stamp: <2024-12-31 17:27:18 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/file/initializers.el

(require '01-llemacs-config)
(require '02-llemacs-logging-core-file)

(defun llemacs--logging-init-file-system ()
  "Initialize file system for structured logging."
  (unless (file-exists-p llemacs--path-logging-logs)
    (make-directory llemacs--path-logging-logs t))
  (dolist (level '(debug info warn error))
    (let ((file-path (expand-file-name
                      (format "%s.log" (symbol-name level))
                      llemacs--path-logging-logs)))
      (unless (file-exists-p file-path)
        (with-temp-file file-path
          (insert "")))))
  (message "Log files initialized at %s" llemacs--path-logging-logs))

(provide '02-llemacs-logging-file-initializers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))