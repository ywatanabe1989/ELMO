;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:55:10
;;; Time-stamp: <2025-01-02 10:55:10 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/file-loggers.el

(defun llemacs--logging-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s: L%s"
              (or load-file-name buffer-file-name "unknown")
              ;; (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--logging-format-message (level message &optional project-id)
  "Format log message with LEVEL, MESSAGE and optional PROJECT-ID, FILE-PATH, and LINE-NUM."
  (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
          llemacs--logging-splitter
          (upcase (symbol-name level))
          (format-time-string "%Y-%m-%d %H:%M:%S")
          (if project-id
              (format "[Project: %s]" project-id)
            "")
          (llemacs--logging-get-caller-info)
          (if (not (stringp message))
              (format "%s" message)
            message)))

(defun llemacs--logging-log (level message &optional project-id)
  "Log MESSAGE at LEVEL with optional PROJECT-ID."
  (let* ((log-entry (llemacs--logging-format-message level message project-id))
         (log-file (cond
                    ((eq level 'error) llemacs--path-logging-system-error)
                    ((eq level 'warn) llemacs--path-logging-system-warn)
                    ((eq level 'info) llemacs--path-logging-system-info)
                    ((eq level 'prompt) llemacs--path-logging-system-prompt)
                    ((eq level 'elisp) llemacs--path-logging-system-elisp)
                    ((eq level 'api) llemacs--path-logging-system-api)
                    ((eq level 'search) llemacs--path-logging-system-search)
                    ((eq level 'debug) llemacs--path-logging-system-debug))))
    (llemacs--logging-system-files-init-if)
    (append-to-file (concat log-entry "\n") nil llemacs--path-logging-system-all)
    (append-to-file (concat log-entry "\n") nil log-file)))

(defun llemacs--logging-log-debug (message &optional project-id)
  "Log debug MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'debug message project-id))
;; (llemacs--logging-log-debug "DEBUG MESSAGE HERE")

(defun llemacs--logging-log-info (message &optional project-id)
  "Log info MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'info message project-id))
;; (llemacs--logging-log-info "INFO MESSAGE HERE")

(defun llemacs--logging-log-search (message &optional project-id)
  "Log search MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'search message project-id))
;; (llemacs--logging-log-search "SEARCH MESSAGE HERE")

(defun llemacs--logging-log-elisp (message &optional project-id)
  "Log elisp MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'elisp message project-id))
;; (llemacs--logging-log-elisp "ELISP MESSAGE HERE")

(defun llemacs--logging-log-prompt (message &optional project-id)
  "Log prompt MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'prompt message project-id))
;; (llemacs--logging-log-prompt "PROMPT MESSAGE HERE")

(defun llemacs--logging-log-api (message &optional project-id)
  "Log api MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'api message project-id))
;; (llemacs--logging-log-api "API MESSAGE HERE")

(defun llemacs--logging-log-warn (message &optional project-id)
  "Log warn MESSAGE with optional PROJECT-ID."
  (llemacs--logging-log 'warn message project-id))
;; (llemacs--logging-log-warn "WARN MESSAGE HERE")

(defun llemacs--logging-log-error (message &optional project-id)
  "Log error MESSAGE with optional PROJECT-ID and open system log file."
  (llemacs--logging-log 'error message project-id)
  (llemacs--buffer-display llemacs--buffer-logging llemacs--path-logging-system-all t))
;; (llemacs--logging-log-error "ERROR MESSAGE HERE")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))