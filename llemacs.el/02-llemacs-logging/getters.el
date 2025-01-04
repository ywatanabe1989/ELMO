;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 09:34:46
;;; Time-stamp: <2025-01-04 09:34:46 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/getters.el

;; (defun llemacs--logging-get-log-entries (file-path &optional n)
;;   "Get last N entries from log FILE-PATH."
;;   (with-temp-buffer
;;     (when (file-exists-p file-path)
;;       (insert-file-contents file-path)
;;       (let ((lines nil))
;;         (goto-char (point-max))
;;         (while (and (or (null n) (< (length lines) n))
;;                     (= 0 (forward-line -1)))
;;           (push (buffer-substring-no-properties
;;                  (line-beginning-position)
;;                  (line-end-position))
;;                 lines))
;;         lines))))

(defun llemacs--logging-parse-entry (raw-text)
  "Convert RAW-TEXT log entry into structured format."
  (let ((entry-parts (split-string raw-text llemacs--logging-splitter t)))
    (when entry-parts
      (car entry-parts))))

(defun llemacs--logging-get-log-entries (file-path &optional n)
  "Get last N entries from log FILE-PATH."
  (with-temp-buffer
    (when (file-exists-p file-path)
      (insert-file-contents file-path)
      (let ((entries nil)
            (current-entry ""))
        (goto-char (point-max))
        (while (and (or (null n) (< (length entries) n))
                    (not (bobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (setq current-entry (concat line "\n" current-entry))
            (when (string-prefix-p llemacs--logging-splitter line)
              (push (string-trim current-entry) entries)
              (setq current-entry "")))
          (forward-line -1))
        entries))))

;; (defun llemacs--logging-get-logs-by-level (level &optional n is-sys)
;;   "Get last N log entries of specified LEVEL.
;; If IS-SYS is non-nil, get system logs instead of project logs."
;;   (when (>= (llemacs--logging-get-level-value level)
;;             (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;;     (let* ((var-name (intern (format "llemacs--path-logs-%s-%s"
;;                                      (symbol-name level)
;;                                      (if is-sys "sys" "pj"))))
;;            (entries (llemacs--logging-get-log-entries (symbol-value var-name) n)))
;;       entries)))
(defun llemacs--logging-get-level-value (level)
  "Get numeric value for log LEVEL."
  (let ((level-sym (if (stringp level) (intern level) level)))
    (cadr (assq level-sym llemacs--log-levels-sys))))

(defun llemacs--logging-get-logs-by-level (level &optional n is-sys)
  "Get last N log entries of specified LEVEL.
If IS-SYS is non-nil, get system logs instead of project logs."
  (let ((level-sym (if (stringp level) (intern level) level)))
    (when (>= (or (llemacs--logging-get-level-value level-sym) 0)
              (llemacs--logging-get-level-value llemacs--logging-level-threshold))
      (let* ((var-name (intern (format "llemacs--path-logs-%s-%s"
                                       (symbol-name level-sym)
                                       (if is-sys "sys" "pj"))))
             (entries (llemacs--logging-get-log-entries (symbol-value var-name) n)))
        entries))))

(defun llemacs--logging-get-logs (&optional level n is-sys)
  "Get last N entries from logs.
If LEVEL is specified, only get that level's logs.
If IS-SYS is non-nil, get system logs instead of project logs."
  (let ((logs nil)
        (levels-list (if is-sys llemacs--log-levels-sys llemacs--log-levels-pj)))
    (if level
        (llemacs--logging-get-logs-by-level level n is-sys)
      (progn
        (dolist (level-entry levels-list)
          (let ((level (car level-entry)))
            (when (>= (llemacs--logging-get-level-value level)
                      (llemacs--logging-get-level-value llemacs--logging-level-threshold))
              (setq logs (append logs (llemacs--logging-get-logs-by-level level n is-sys))))))
        (sort logs #'string>)))))

(defun llemacs--logging-get-logs-sys (&optional level n)
  "Get system logs of LEVEL (last N entries)."
  (llemacs--logging-get-logs level n t))

(defun llemacs--logging-get-logs-pj (&optional level n)
  "Get project logs of LEVEL (last N entries)."
  (llemacs--logging-get-logs level n nil))

;; (llemacs--logging-get-logs-sys 'error) ;; nil
;; (llemacs--logging-get-logs-pj 'error) ;; nil

;; (llemacs--logging-get-logs-sys "error") ;; nil
;; (llemacs--logging-get-logs-pj "error") ;; nil







;; (defun llemacs--logging-get-log-entries (file-path &optional n)
;;   "Get last N entries from log FILE-PATH."
;;   (with-temp-buffer
;;     (when (file-exists-p file-path)
;;       (insert-file-contents file-path)
;;       (let ((lines nil))
;;         (goto-char (point-max))
;;         (while (and (or (null n) (< (length lines) n))
;;                     (= 0 (forward-line -1)))
;;           (push (buffer-substring-no-properties
;;                  (line-beginning-position)
;;                  (line-end-position))
;;                 lines))
;;         lines))))

;; (defun llemacs--logging-get-logs-by-level(level &optional n is-sys)
;;   "Get last N log entries of specified LEVEL.
;; If IS-SYS is non-nil, get system logs instead of project logs."
;;   (when (>= (llemacs--logging-get-level-value level)
;;             (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;;     (let* ((var-name (intern (format "llemacs--path-logs-%s-%s"
;;                                      level
;;                                      (if is-sys "sys" "pj"))))
;;            (entries (llemacs--logging-get-log-entries (symbol-value var-name) n)))
;;       entries)))

;; (defun llemacs--logging-get-logs (&optional level n is-sys)
;;   "Get last N entries from logs.
;; If LEVEL is specified, only get that level's logs.
;; If IS-SYS is non-nil, get system logs instead of project logs."
;;   (if level
;;       (llemacs--logging-get-logs-by-level level n is-sys)
;;     (let ((logs nil)
;;           (levels-list (if is-sys llemacs--log-levels-sys llemacs--log-levels-pj)))
;;       (dolist (level-entry levels-list)
;;         (let ((level (car level-entry)))
;;           (when (>= (llemacs--logging-get-level-value level)
;;                     (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;;             (setq logs (append logs (llemacs--logging-get-logs-by-level level n is-sys))))))
;;       (sort logs #'string>))))

;; (defun llemacs--logging-get-logs-sys (&optional level n)
;;   "Get system logs of LEVEL (last N entries)."
;;   (llemacs--logging-get-logs level n t))

;; (defun llemacs--logging-get-logs-pj (&optional level n)
;;   "Get project logs of LEVEL (last N entries)."
;;   (llemacs--logging-get-logs level n nil))

;; ;; (defun llemacs--logging-get-log-entries (file-path &optional n)
;; ;;   "Get last N entries from log FILE-PATH."
;; ;;   (with-temp-buffer
;; ;;     (when (file-exists-p file-path)
;; ;;       (insert-file-contents file-path)
;; ;;       (let ((lines nil))
;; ;;         (goto-char (point-max))
;; ;;         (while (and (or (null n) (< (length lines) n))
;; ;;                     (= 0 (forward-line -1)))
;; ;;           (push (buffer-substring-no-properties
;; ;;                  (line-beginning-position)
;; ;;                  (line-end-position))
;; ;;                 lines))
;; ;;         lines))))

;; ;; (defun llemacs--logging-get-logs-by-level (level &optional n)
;; ;;   "Get last N log entries of specified LEVEL."
;; ;;   (let* ((file-path (expand-file-name
;; ;;                      (format "%s.log" (symbol-name level))
;; ;;                      llemacs--path-logging-system-logs))
;; ;;          (entries (llemacs--logging-get-log-entries file-path n)))
;; ;;     entries))

;; ;; (defun llemacs--logging-get-all-logs (&optional n)
;; ;;   "Get last N entries from all log files."
;; ;;   (let ((logs nil))
;; ;;     (dolist (level '(debug info warn error))
;; ;;       (setq logs
;; ;;             (append logs
;; ;;                     (llemacs--logging-get-logs-by-level level n))))
;; ;;     (sort logs #'string>)))

;; ;; Considering the level
;; (defun llemacs--logging-get-log-entries (file-path &optional n)
;;   "Get last N entries from log FILE-PATH."
;;   (with-temp-buffer
;;     (when (file-exists-p file-path)
;;       (insert-file-contents file-path)
;;       (let ((lines nil))
;;         (goto-char (point-max))
;;         (while (and (or (null n) (< (length lines) n))
;;                     (= 0 (forward-line -1)))
;;           (push (buffer-substring-no-properties
;;                  (line-beginning-position)
;;                  (line-end-position))
;;                 lines))
;;         lines))))

;; (defun llemacs--logging-get-logs-by-level (level &optional n)
;;   "Get last N log entries of specified LEVEL."
;;   (when (>= (llemacs--logging-get-level-value level)
;;             (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;;     (let* ((file-path (expand-file-name
;;                        (format "%s.log" (symbol-name level))
;;                        llemacs--path-logs-by-level-sys))
;;            (entries (llemacs--logging-get-log-entries file-path n)))
;;       entries)))

;; ;; (defun llemacs--logging-get-all-logs (&optional n)
;; ;;   "Get last N entries from all log files."
;; ;;   (let ((logs nil))
;; ;;     (dolist (level-entry llemacs--log-levels-sys)
;; ;;       (let ((level (car level-entry)))
;; ;;         (when (>= (llemacs--logging-get-level-value level)
;; ;;                   (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;; ;;           (setq logs (append logs (llemacs--logging-get-logs-by-level level n))))))
;; ;;     (sort logs #'string>)))

;; (defun llemacs--logging-get-logs (&optional level n)
;;   "Get last N entries from all log files."
;;   when level llemacs--logging-get-logs-by-level
;;   (let ((logs nil))
;;     (dolist (level-entry llemacs--log-levels-sys)
;;       (let ((level (car level-entry)))
;;         (when (>= (llemacs--logging-get-level-value level)
;;                   (llemacs--logging-get-level-value llemacs--logging-level-threshold))
;;           (setq logs (append logs (llemacs--logging-get-logs-by-level level n))))))
;;     (sort logs #'string>)))

;; ;; (llemacs-list "variable" "llemacs--path-logs")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))