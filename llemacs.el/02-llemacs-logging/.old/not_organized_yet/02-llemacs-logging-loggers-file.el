;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:52:01
;;; Time-stamp: <2024-12-31 16:52:01 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-loggers-file.el

(require '02-llemacs-logging-db)

(defvar llemacs--logging-level-threshold 'info
  "Minimum log level to record.")

(defun llemacs--logging-level-value (level)
  "Convert log LEVEL to numeric value for comparison."
  (pcase level
    ('debug 0)
    ('info 1)
    ('search 1)
    ('api 1)
    ('prompt 1)
    ('warn 2)
    ('success 3)
    ('error 4)
    (_ 1)))

(defcustom llemacs-logging-max-size 10485760
  "Maximum size of log files in bytes (10MB default)."
  :type 'integer
  :group 'llemacs-logging)

(defcustom llemacs-logging-backup-count 5
  "Number of log backup files to keep."
  :type 'integer
  :group 'llemacs-logging)


;; ----------------------------------------
;; File-based loggers: For general system events, debugging, errors (like traditional log files)
;; ----------------------------------------
(defun llemacs--logging-rotate-if-needed (file)
  "Rotate FILE if it exceeds maximum size by appending timestamp."
  (when (and (file-exists-p file)
             (> (file-attribute-size (file-attributes file))
                llemacs-logging-max-size))
    (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (base (file-name-sans-extension file))
           (ext (file-name-extension file)))
      (rename-file file (format "%s-%s.%s" base timestamp ext) t))))

(defun llemacs--logging-log-to-file (message file)
  "Write MESSAGE to specified log FILE."
  (with-temp-buffer
    (insert message "\n")
    (llemacs--logging-rotate-if-needed file)
    (append-to-file (point-min) (point-max) file)))

(defun llemacs--logging-format-message (level message)
  "Format log message with LEVEL and MESSAGE."
  (format "[%s][%s][%s] %s"
          (format-time-string "%Y-%m-%d %H:%M:%S")
          (upcase (symbol-name level))
          (llemacs--log-get-caller-info)
          message))

(defun llemacs--log-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s:%s"
              (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--logging-write (level message &optional file)
  "Write log with LEVEL and MESSAGE to optional FILE."
  (when (<= (llemacs--logging-level-value llemacs--logging-level-threshold)
            (llemacs--logging-level-value level))
    (let ((log-file (or file llemacs--path-log-system)))
      (llemacs--logging-rotate-if-needed log-file)
      (llemacs--logging-log-to-file
       (llemacs--logging-format-message level message)
       log-file)))
  (when (memq level '(error warn search api prompt))
    (llemacs--logging-open-log-file)))

(defun llemacs--logging-debug (message)
  "Log debug MESSAGE."
  (llemacs--logging-write 'debug message))

(defun llemacs--logging-info (message)
  "Log info MESSAGE."
  (llemacs--logging-write 'info message))

(defun llemacs--logging-search (query)
  "Log search QUERY."
  (llemacs--logging-write 'search query))

(defun llemacs--logging-api-call (provider method)
  "Log API call to PROVIDER using METHOD."
  (llemacs--logging-write 'api (format "%s - %s" provider method)))

(defun llemacs--logging-prompt (prompt)
  "Log user PROMPT."
  (llemacs--logging-write 'prompt prompt))

(defun llemacs--logging-warn (message)
  "Log warning MESSAGE."
  (llemacs--logging-write 'warn message))

(defun llemacs--logging-error (message)
  "Log error MESSAGE."
  (llemacs--logging-write 'error message))

(defun llemacs--logging-success (message)
  "Log success MESSAGE."
  (llemacs--logging-write 'success message))

(defun llemacs--logging-open-log-file ()
  "Open the log file in a split window."
  (let* ((log-buffer (find-file-noselect llemacs--path-log-system))
         (window (split-window-below -10)))
    (set-window-buffer window log-buffer)
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (recenter -1))
    (select-window window)))

;; (llemacs--logging-info "aaa")
;; (llemacs--logging-success "aaa")
;; (llemacs--logging-error "aaa")
;; (llemacs--logging-open-log-file)

(provide '02-llemacs-logging-loggers-file)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))