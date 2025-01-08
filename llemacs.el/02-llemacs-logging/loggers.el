;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 02:34:56
;;; Timestamp: <2025-01-09 02:34:56>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/loggers.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--sanitize-filepath (path)
  "Sanitize file path while preserving directory structure."
  (when path
    (replace-regexp-in-string "[\\<>\"';`$]" "_" path)))

(defun llemacs--sanitize-content (content)
  "Sanitize content for LLM compatibility while preserving special chars."
  (when content
    (replace-regexp-in-string "\r\n" "\n" (format "%s" content))))

(defun llemacs--logging-get-caller-info ()
  "Get caller's location info (file and line number where logger was called)."
  (let* ((frames (backtrace-frames))
         (target-frame (cl-find-if
                        (lambda (frame)
                          (and (car frame)
                               (cadr frame)
                               (not (string-match "^llemacs--logging"
                                                  (symbol-name (cadr frame))))))
                        frames))
         (func-name (if (and target-frame (symbolp (cadr target-frame)))
                        (symbol-name (cadr target-frame))
                      "unknown"))
         (frame-data (and target-frame (cdr target-frame)))
         (line-number (if (and frame-data (integerp (car frame-data)))
                          (number-to-string (car frame-data))
                        "0")))
    (format "%s: L%s" func-name line-number)))

(defun llemacs--logging-format-message (level message &optional full-project-name)
  "Format log MESSAGE with LEVEL and optional FULL-PROJECT-NAME."
  (let ((full-project-name (if (and full-project-name (string-match "^\\([0-9]+\\)-" full-project-name))
                               (match-string 1 full-project-name)
                             full-project-name)))
    (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
            llemacs--logging-splitter
            (upcase (symbol-name level))
            (llemacs-timestamp)
            (if full-project-name (format "[Project: %s]" full-project-name) "")
            (llemacs--logging-get-caller-info)
            (or message "No message"))))

(defcustom llemacs--logging-enable-display-threshold 'error
  "Threshold level for displaying log messages. Logs at this level and above will be displayed."
  :type 'symbol)

(defun llemacs--logging-meets-threshold-p (level)
  "Check if LEVEL meets or exceeds the display threshold."
  (>= (cadr (assq level llemacs--log-levels-sys))
      (cadr (assq llemacs--logging-enable-display-threshold
                  llemacs--log-levels-sys))))

(defun llemacs--logging-ensure-log-file (file-path)
  "Ensure log file and its directory exist."
  (let ((dir (file-name-directory (expand-file-name (llemacs--sanitize-filepath file-path)))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p file-path)
      (write-region "" nil file-path nil 'quiet))))

(defun llemacs--logging-write-quiet (content file-path &optional append)
  "Write CONTENT to FILE-PATH quietly, with optional APPEND."
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert (llemacs--sanitize-content content))
    (write-region (point-min) (point-max)
                  (llemacs--sanitize-filepath file-path)
                  append 'quiet)))

(defun llemacs--logging-write (level message &optional project-id enable-display)
  "Log MESSAGE at LEVEL with optional PROJECT-ID."
  (let* ((project-id (or project-id llemacs--cur-pj))
         (is-system-log (or (null project-id)
                            (string= project-id "sys")))
         (log-entry (llemacs--logging-format-message level message (unless is-system-log project-id)))
         (log-file-level
          (if is-system-log
              (let ((var-name (intern (format "llemacs--path-logs-%s-sys" level))))
                (symbol-value var-name))
            (let ((var-name (intern (format "llemacs--path-logs-%s-pj" level))))
              (symbol-value var-name))))
         (log-file-all (if is-system-log
                           llemacs--path-logs-all-sys
                         llemacs--path-pj-logs-all)))
    (llemacs--logging-ensure-log-file log-file-all)
    (llemacs--logging-ensure-log-file log-file-level)
    (llemacs--logging-write-quiet (concat log-entry "\n") log-file-all t)
    (llemacs--logging-write-quiet (concat log-entry "\n") log-file-level t)
    (when (and enable-display
               (llemacs--logging-meets-threshold-p level))
      (message log-entry))))

(defun llemacs--logging-define-loggers-sys ()
  "Define system-level logging functions."
  (dolist (level-config llemacs--log-levels-sys)
    (let ((level (car level-config))
          (priority (cadr level-config)))
      (eval
       `(defun ,(intern (format "llemacs--logging-write-%s-sys" level)) (message &optional project-id)
          ,(format "Log %s MESSAGE" level)
          (llemacs--logging-write ',level message project-id ,(= priority 3)))))))

(defun llemacs--logging-define-loggers-pj ()
  "Define project-level logging functions."
  (dolist (level-config llemacs--log-levels-pj)
    (let ((level (car level-config))
          (priority (cadr level-config)))
      (eval
       `(defun ,(intern (format "llemacs--logging-write-%s-pj" level)) (message &optional project-id)
          ,(format "Log %s MESSAGE with PROJECT-ID." level)
          (llemacs--logging-write ',level message project-id ,(= priority 3))
          (funcall (intern (format "llemacs--logging-write-%s-sys" ',level)) message))))))

(llemacs--logging-define-loggers-sys)
(llemacs--logging-define-loggers-pj)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
