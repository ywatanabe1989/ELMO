;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:34:28
;;; Timestamp: <2025-01-11 08:34:28>
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

;; (defun llemacs--sanitize-content (content)
;;   "Sanitize content for LLM compatibility while preserving special chars."
;;   (when content
;;     (replace-regexp-in-string "\r\n" "\n" (format "%s" content))))

(defun llemacs--sanitize-content (content)
  "Sanitize content for LLM compatibility while preserving special chars."
  (when content
    (let ((print-escape-newlines nil)
          (print-escape-nonascii t))
      (format "%S" content))))

;; L0 but working
(defun llemacs--logging-get-caller-info ()
  "Get caller's location info (file and line number where logger was called)."
  (let ((i 4)
        (found nil))
    (while (and (not found) (< i 20))
      (let* ((frame (backtrace-frame i))
             (func (and frame (cadr frame))))
        (when (and func
                   (symbolp func)
                   (not (string-prefix-p "llemacs--logging" (symbol-name func))))
          (setq found func))
        (setq i (1+ i))))
    (format "%s: L%d"
            (if found (symbol-name found) "unknown")
            0)))

(defun llemacs--logging-format-message (level message &optional full-project-name)
  "Format log MESSAGE with LEVEL and optional FULL-PROJECT-NAME."
  (format "%s\n[%s][%s]\n=> %s\n%s"
          llemacs--logging-splitter
          (upcase (symbol-name level))
          (llemacs-timestamp)
          ;; (format "[Project: %s]" full-project-name)
          (llemacs--logging-get-caller-info)
          (or message "No message")))

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

;; (defun llemacs--logging-write (level message &optional project-id enable-display)
;;   "Log MESSAGE at LEVEL with optional PROJECT-ID."
;;   (let* ((project-id (or project-id llemacs--cur-pj))
;;          (is-system-log (or (null project-id)
;;                             (string= project-id "sys")))
;;          (log-entry (llemacs--logging-format-message level message (unless is-system-log project-id)))
;;          (log-file-level
;;           (if is-system-log
;;               (let ((var-name (intern (format "llemacs--path-logs-%s-sys" level))))
;;                 (symbol-value var-name))
;;             (let ((var-name (intern (format "llemacs--path-logs-%s-pj" level))))
;;               (symbol-value var-name))))
;;          (log-file-all (if is-system-log
;;                            llemacs--path-logs-all-sys
;;                          llemacs--path-pj-logs-all)))
;;     (llemacs--logging-ensure-log-file log-file-all)
;;     (llemacs--logging-ensure-log-file log-file-level)
;;     (llemacs--logging-write-quiet (concat log-entry "\n") log-file-all t)
;;     (llemacs--logging-write-quiet (concat log-entry "\n") log-file-level t)
;;     (when (and enable-display
;;                (llemacs--logging-meets-threshold-p level))
;;       (message log-entry))))


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
                         llemacs--path-pj-logs-all))
         (log-file-main (if is-system-log
                            llemacs--path-logs-main-sys
                          llemacs--path-pj-logs-main)))
    (llemacs--logging-ensure-log-file log-file-all)
    (llemacs--logging-ensure-log-file log-file-level)
    (llemacs--logging-ensure-log-file log-file-main)
    (llemacs--logging-write-quiet (concat log-entry "\n") log-file-all t)
    (when (llemacs--logging-meets-main-log-threshold-p level)
      (llemacs--logging-write-quiet (concat log-entry "\n") log-file-main t))
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

(defun llemacs--logging-write-error-pj (string &rest args)
  "Log error message formatted from STRING and ARGS.
Like built-in error function but logs instead of signaling.

Example 1: Simple string
(llemacs--logging-write-error-pj \"Something went wrong\")

Example 2: With format arguments
(llemacs--logging-write-error-pj \"Failed to process %s at line %d\" \"file.txt\" 42)
"
  (let ((inhibit-quit t))
    (with-demoted-errors "Logging error: %S"
      (let ((message (apply #'format string args)))
        (llemacs--logging-write 'error message nil t)))))

(defun llemacs--logging-write-warn-pj (message &optional project-id)
  "Log WARN MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'warn message project-id nil))

(defun llemacs--logging-write-info-pj (message &optional project-id)
  "Log INFO MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'info message project-id nil))

(defun llemacs--logging-write-success-pj (message &optional project-id)
  "Log SUCCESS MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'success message project-id nil))

(defun llemacs--logging-write-prompt-pj (message &optional project-id)
  "Log PROMPT MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'prompt message project-id nil))

(defun llemacs--logging-write-elisp-pj (message &optional project-id)
  "Log ELISP MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'elisp message project-id nil))

(defun llemacs--logging-write-api-pj (message &optional project-id)
  "Log API MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'api message project-id nil))

(defun llemacs--logging-write-search-pj (message &optional project-id)
  "Log SEARCH MESSAGE with PROJECT-ID."
  (llemacs--logging-write 'search message project-id nil))

(defun llemacs--logging-write-debug-pj (message &optional project-id)
"Log DEBUG MESSAGE with PROJECT-ID."
(llemacs--logging-write 'debug message project-id nil))

;; (llemacs--logging-write-error-pj "aaa")
;; (message "from here")
;; (llemacs--logging-write-error-pj "085-Epilepsy-prediction-project")

;; (defun llemacs--logging-define-loggers-pj ()
;;   "Define project-level logging functions."
;;   (dolist (level-config llemacs--log-levels-pj)
;;     (let ((level (car level-config))
;;           (priority (cadr level-config)))
;;       (eval
;;        `(defun ,(intern (format "llemacs--logging-write-%s-pj" level)) (message &optional project-id)
;;           ,(format "Log %s MESSAGE with PROJECT-ID." level)
;;           (llemacs--logging-write ',level message project-id ,(= priority 3))
;;           (funcall (intern (format "llemacs--logging-write-%s-sys" ',level)) message))))))

;; (llemacs--logging-define-loggers-sys)
;; (llemacs--logging-define-loggers-pj)



(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
