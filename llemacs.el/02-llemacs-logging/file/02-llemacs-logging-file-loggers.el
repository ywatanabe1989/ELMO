;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:23:40
;;; Time-stamp: <2024-12-31 17:23:40 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/file/loggers.el

(require '02-llemacs-logging-core-utils)
(require '02-llemacs-logging-core-file)
(require '02-llemacs-logging-file-initializers)

(cl-defun llemacs--logging-log (level message &key project-id file-path line-num)
  "Log MESSAGE at LEVEL with optional PROJECT-ID, FILE-PATH, and LINE-NUM."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (log-entry (format "[%s] [%s] %s%s%s: %s"
                            timestamp
                            (upcase (symbol-name level))
                            (if project-id
                                (format "[Project: %s] " project-id)
                              "")
                            (if file-path
                                (format "[File: %s] " file-path)
                              "")
                            (if line-num
                                (format "[Line: %d] " line-num)
                              "")
                            message))
         (log-file (expand-file-name
                    (format "%s.log" (symbol-name level))
                    llemacs--path-logging-logs)))
    (unless (file-exists-p llemacs--path-logging-logs)
      (llemacs--logging-init-file-system))
    (append-to-file (concat log-entry "\n") nil log-file)))

(defalias 'llemacs--logging-debug
  (lambda (message &rest args)
    (llemacs--logging-log 'debug (apply #'format message args)))
  "Log debug message.")

(defalias 'llemacs--logging-info
  (lambda (message &rest args)
    (llemacs--logging-log 'info (apply #'format message args)))
  "Log info message.")

(defalias 'llemacs--logging-warn
  (lambda (message &rest args)
    (llemacs--logging-log 'warn (apply #'format message args)))
  "Log warning message.")

(defalias 'llemacs--logging-error
  (lambda (message &rest args)
    (llemacs--logging-log 'error (apply #'format message args)))
  "Log error message.")

(provide '02-llemacs-logging-file-loggers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))