;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 02:49:33
;;; Time-stamp: <2025-01-03 02:49:33 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/file-loggers.el

(defun llemacs--logging-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s: L%s"
              (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--logging-get-timestamp ()
  "Formated timestamp for consistency."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun llemacs--logging-format-message (level message &optional project-id-or-full-project-name)
  "Format log message with LEVEL, MESSAGE and optional PROJECT-ID, FILE-PATH, and LINE-NUM."
  (let ((project-id (if (and project-id-or-full-project-name
                             (string-match "^\\([0-9]+\\)-" project-id-or-full-project-name))
                        (match-string 1 project-id-or-full-project-name)
                      project-id-or-full-project-name)))
    (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
            llemacs--logging-splitter
            (upcase (symbol-name level))
            (llemacs--loggin-get-timestamp)
            (if project-id
                (format "[Project: %s]" project-id)
              "")
            (llemacs--logging-get-caller-info)
            (if (not (stringp message))
                (format "%s" message)
              message))))

;; (defun llemacs--logging-format-message (level message &optional project-id)
;;   "Format log message with LEVEL, MESSAGE and optional PROJECT-ID, FILE-PATH, and LINE-NUM."
;;   (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
;;           llemacs--logging-splitter
;;           (upcase (symbol-name level))
;;           (llemacs--loggin-get-timestamp)
;;           (if project-id
;;               (format "[Project: %s]" project-id)
;;             "")
;;           (llemacs--logging-get-caller-info)
;;           (if (not (stringp message))
;;               (format "%s" message)
;;             message)))

(defun llemacs--logging-log (level message &optional project-id-or-full-name)
  "Log MESSAGE at LEVEL with optional PROJECT-ID-OR-FULL-NAME."
  (let* ((log-entry (llemacs--logging-format-message level message project-id-or-full-name))
         (log-file
          (if project-id-or-full-name
              (pcase level
                ('error (llemacs--path-project-error-get project-id-or-full-name))
                ('warn (llemacs--path-project-warn-get project-id-or-full-name))
                ('info (llemacs--path-project-info-get project-id-or-full-name))
                ('success (llemacs--path-project-success-get project-id-or-full-name))
                ('debug (llemacs--path-project-debug-get project-id-or-full-name)))
            (pcase level
              ('error llemacs--path-logging-system-error)
              ('warn llemacs--path-logging-system-warn)
              ('info llemacs--path-logging-system-info)
              ('success llemacs--path-logging-system-success)
              ('prompt llemacs--path-logging-system-prompt)
              ('elisp llemacs--path-logging-system-elisp)
              ('api llemacs--path-logging-system-api)
              ('search llemacs--path-logging-system-search)
              ('debug llemacs--path-logging-system-debug))))
         (all-log-file (if project-id-or-full-name
                           (llemacs--path-project-all-get project-id-or-full-name)
                         llemacs--path-logging-system-all)))
    (llemacs--logging-system-files-init-if)
    (append-to-file (concat log-entry "\n") nil all-log-file)
    (append-to-file (concat log-entry "\n") nil log-file)))

(defun llemacs--logging-log-debug (message &optional project-id-or-full-name)
  "Log debug MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'debug message project-id-or-full-name))
;; (llemacs--logging-log-debug "DEBUG MESSAGE HERE")
;; (llemacs--logging-log-debug "DEBUG MESSAGE HERE" "026-my-first-project")

(defun llemacs--logging-log-info (message &optional project-id-or-full-name)
  "Log info MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'info message project-id-or-full-name))
;; (llemacs--logging-log-info "INFO MESSAGE HERE")

(defun llemacs--logging-log-success (message &optional project-id-or-full-name)
  "Log success MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'success message project-id-or-full-name))
;; (llemacs--logging-log-success "SUCCESS MESSAGE HERE")

(defun llemacs--logging-log-search (message &optional project-id-or-full-name)
  "Log search MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'search message project-id-or-full-name))
;; (llemacs--logging-log-search "SEARCH MESSAGE HERE")

(defun llemacs--logging-log-elisp (message &optional project-id-or-full-name)
  "Log elisp MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'elisp message project-id-or-full-name))
;; (llemacs--logging-log-elisp "ELISP MESSAGE HERE")

(defun llemacs--logging-log-prompt (message &optional project-id-or-full-name)
  "Log prompt MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'prompt message project-id-or-full-name))
;; (llemacs--logging-log-prompt "PROMPT MESSAGE HERE")

(defun llemacs--logging-log-api (message &optional project-id-or-full-name)
  "Log api MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'api message project-id-or-full-name))
;; (llemacs--logging-log-api "API MESSAGE HERE")

(defun llemacs--logging-log-warn (message &optional project-id-or-full-name)
  "Log warn MESSAGE with optional PROJECT-ID-OR-FULL-NAME."
  (llemacs--logging-log 'warn message project-id-or-full-name))
;; (llemacs--logging-log-warn "WARN MESSAGE HERE")

(defun llemacs--logging-log-error (message &optional project-id-or-full-name)
  "Log error MESSAGE with optional PROJECT-ID-OR-FULL-NAME and open system log file."
  (llemacs--logging-log 'error message project-id-or-full-name)
  (llemacs--buffer-display llemacs--buffer-logging llemacs--path-logging-system-all t))
;; (llemacs--logging-log-error "ERROR MESSAGE HERE")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))