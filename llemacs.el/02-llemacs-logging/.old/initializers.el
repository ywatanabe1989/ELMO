;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:48:28
;;; Time-stamp: <2025-01-04 12:48:28 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/initializers.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2025-01-02 17:54:22
;; ;;; Time-stamp: <2025-01-02 17:54:22 (ywatanabe)>
;; ;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/file-initializers.el

;; (defun llemacs--logging-system-files-init-if ()
;;   "Initialize file system for structured logging."
;;   (unless (file-exists-p llemacs--path-logging-system-logs)
;;     (make-directory llemacs--path-logging-system-logs t))
;;   (unless (file-exists-p llemacs--path-logging-system-logs-by-level)
;;     (make-directory llemacs--path-logging-system-logs-by-level t))
;;   (unless (file-exists-p llemacs--path-logging-system-all)
;;     (with-temp-file llemacs--path-logging-system-all
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-error)
;;     (with-temp-file llemacs--path-logging-system-error
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-warn)
;;     (with-temp-file llemacs--path-logging-system-warn
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-info)
;;     (with-temp-file llemacs--path-logging-system-info
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-search)
;;     (with-temp-file llemacs--path-logging-system-search
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-prompt)
;;     (with-temp-file llemacs--path-logging-system-prompt
;;       (insert "")
;;       (save-buffer)))
;;   (unless (file-exists-p llemacs--path-logging-system-debug)
;;     (with-temp-file llemacs--path-logging-system-debug
;;       (insert "")
;;       (save-buffer)))
;;   (message "Log files initialized at %s" llemacs--path-logging-system-logs))

;; ;; (llemacs--logging-system-files-init-if)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))