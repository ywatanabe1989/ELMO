;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:49:37
;;; Time-stamp: <2025-01-04 12:49:37 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/maintainers.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2025-01-04 09:02:19
;; ;;; Time-stamp: <2025-01-04 09:02:19 (ywatanabe)>
;; ;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/maintainers.el

;; (defun llemacs--logging-rotate-logs ()
;;   "Rotate log files by appending timestamp to old files."
;;   (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
;;     (dolist (level '(debug info warn error))
;;       (let* ((log-file (expand-file-name
;;                         (format "%s.log" (symbol-name level))
;;                         llemacs--path-logging-system-logs))
;;              (archived-file (expand-file-name
;;                              (format "%s-%s.log" (symbol-name level) timestamp)
;;                              llemacs--path-logging-system-logs)))
;;         (when (file-exists-p log-file)
;;           (rename-file log-file archived-file)
;;           (with-temp-file log-file
;;             (insert "")))))))

;; (defun llemacs--logging-clear-old-logs (days)
;;   "Remove log files older than DAYS."
;;   (let* ((current-time (current-time))
;;          (cutoff-time (time-subtract current-time (days-to-time days))))
;;     (dolist (file (directory-files llemacs--path-logging-system-logs t "\\.log"))
;;       (when (time-less-p
;;              (nth 5 (file-attributes file))
;;              cutoff-time)
;;         (delete-file file)))))

;; (defun llemacs--logging-compress-old-logs (days)
;;   "Compress log files older than DAYS."
;;   (let* ((current-time (current-time))
;;          (cutoff-time (time-subtract current-time (days-to-time days))))
;;     (dolist (file (directory-files llemacs--path-logging-system-logs t "\\.log$"))
;;       (when (and (time-less-p
;;                   (nth 5 (file-attributes file))
;;                   cutoff-time)
;;                  (not (string-match "\\.gz$" file)))
;;         (with-temp-buffer
;;           (insert-file-contents file)
;;           (write-region (point-min) (point-max) (concat file ".gz") nil nil nil 'gzip)
;;           (delete-file file))))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))