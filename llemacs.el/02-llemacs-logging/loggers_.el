;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 00:10:28
;;; Time-stamp: <2025-01-05 00:10:28 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/loggers.el

(defun llemacs--logging-get-caller-info ()
  "Get caller's file and line info."
  (let ((caller-file (or (or load-file-name buffer-file-name) "unknown"))
        (caller-line (or (line-number-at-pos) 0)))
    (format "%s: L%d" caller-file caller-line)))

;; (defun llemacs--sanitize-text (text)
;;   "Sanitize TEXT to ensure safe file writing."
;;   (when text
;;     (replace-regexp-in-string
;;      "\r\n" "\n"
;;      (replace-regexp-in-string
;;       "[^[:ascii:]]\\|[\\<>\"';`$]"
;;       "_"
;;       (format "%s" text)))))

(defun llemacs--sanitize-text (text)
  "Sanitize TEXT to ensure safe file writing."
  (when text
    (replace-regexp-in-string
     "\r\n" "\n"
     (replace-regexp-in-string
      "[^[:ascii:]]\\|[\\<>\"';`$]"
      "_"
      (format "%s" text)))))

;; (defun llemacs--logging-format-message (level message &optional project-id)
;;   "Format log MESSAGE with LEVEL and optional PROJECT-ID."
;;   (let ((project-id (if (and project-id (string-match "^\\([0-9]+\\)-" project-id))
;;                         (match-string 1 project-id)
;;                       project-id)))
;;     (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
;;             llemacs--logging-splitter
;;             (upcase (symbol-name level))
;;             (llemacs-timestamp)
;;             (if project-id (format "[Project: %s]" (llemacs--sanitize-text project-id)) "")
;;             (llemacs--logging-get-caller-info)
;;             (llemacs--sanitize-text (or message "No message")))))

(defun llemacs--logging-format-message (level message &optional project-id)
  "Format log MESSAGE with LEVEL and optional PROJECT-ID."
  (let ((project-id (if (and project-id (string-match "^\\([0-9]+\\)-" project-id))
                        (match-string 1 project-id)
                      project-id)))
    (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
            llemacs--logging-splitter
            (upcase (symbol-name level))
            (llemacs-timestamp)
            (if project-id (format "[Project: %s]" project-id) "")
            (llemacs--logging-get-caller-info)
            (or message "No message"))))

;; (defun llemacs--logging-format-message (level message &optional project-id)
;;   "Format log MESSAGE with LEVEL and optional PROJECT-ID."
;;   (let ((project-id (if (and project-id (string-match "^\\([0-9]+\\)-" project-id))
;;                         (match-string 1 project-id)
;;                       project-id)))
;;     (format "%s\n[%s LOG]\n[%s]%s\n=> %s\n%s"
;;             llemacs--logging-splitter
;;             (upcase (symbol-name level))
;;             (llemacs-timestamp)
;;             (if project-id (format "[Project: %s]" project-id) "")
;;             (llemacs--logging-get-caller-info)
;;             (format "%s" (or message "No message")))))

(defcustom llemacs--logging-enable-display-threshold 'error
  "Threshold level for displaying log messages. Logs at this level and above will be displayed."
  :type 'symbol)

(defun llemacs--logging-meets-threshold-p (level)
  "Check if LEVEL meets or exceeds the display threshold."
  (>= (cadr (assq level llemacs--log-levels-sys))
      (cadr (assq llemacs--logging-enable-display-threshold
                  llemacs--log-levels-sys))))

;; (defun llemacs--logging-ensure-log-file (file-path)
;;   "Ensure log file and its directory exist."
;;   (let ((dir (file-name-directory file-path)))
;;     (unless (file-exists-p dir)
;;       (make-directory dir t))
;;     (unless (file-exists-p file-path)
;;       (write-region "" nil file-path))))
(defun llemacs--logging-ensure-log-file (file-path)
  "Ensure log file and its directory exist."
  (let ((dir (file-name-directory (expand-file-name file-path))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p file-path)
      (write-region "" nil file-path nil 'quiet))))

(defun llemacs--logging-write-quiet (content file-path &optional append)
  "Write CONTENT to FILE-PATH quietly, with optional APPEND."
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert (llemacs--sanitize-text content))
    (write-region (point-min) (point-max) file-path append 'quiet)))

;; (defun llemacs--logging-write-quiet (content file-path &optional append)
;;   "Write CONTENT to FILE-PATH quietly, with optional APPEND."
;;   (with-temp-buffer
;;     (insert content)
;;     (write-region (point-min) (point-max) file-path append 'quiet)))

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