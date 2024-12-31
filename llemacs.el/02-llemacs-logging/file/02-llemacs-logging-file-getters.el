;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:57:13
;;; Time-stamp: <2024-12-31 17:57:13 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/file/02-llemacs-logging-file-getters.el

(require '02-llemacs-logging-core-file)

(defun llemacs--logging-get-log-entries (file-path &optional n)
  "Get last N entries from log FILE-PATH."
  (with-temp-buffer
    (when (file-exists-p file-path)
      (insert-file-contents file-path)
      (let ((lines nil))
        (goto-char (point-max))
        (while (and (or (null n) (< (length lines) n))
                    (= 0 (forward-line -1)))
          (push (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))
                lines))
        lines))))

(defun llemacs--logging-get-logs-by-level (level &optional n)
  "Get last N log entries of specified LEVEL."
  (let* ((file-path (expand-file-name
                     (format "%s.log" (symbol-name level))
                     llemacs--path-logging-logs))
         (entries (llemacs--logging-get-log-entries file-path n)))
    entries))

(defun llemacs--logging-get-all-logs (&optional n)
  "Get last N entries from all log files."
  (let ((logs nil))
    (dolist (level '(debug info warn error))
      (setq logs
            (append logs
                    (llemacs--logging-get-logs-by-level level n))))
    (sort logs #'string>)))

(provide '02-llemacs-logging-file-getters)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))