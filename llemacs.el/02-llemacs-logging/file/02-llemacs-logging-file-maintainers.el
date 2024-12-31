;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:23:40
;;; Time-stamp: <2024-12-31 17:23:40 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/file/maintainers.el

(require '02-llemacs-logging-core-file)

(defun llemacs--logging-rotate-logs ()
  "Rotate log files by appending timestamp to old files."
  (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
    (dolist (level '(debug info warn error))
      (let* ((log-file (expand-file-name
                        (format "%s.log" (symbol-name level))
                        llemacs--path-logging-logs))
             (archived-file (expand-file-name
                             (format "%s-%s.log" (symbol-name level) timestamp)
                             llemacs--path-logging-logs)))
        (when (file-exists-p log-file)
          (rename-file log-file archived-file)
          (with-temp-file log-file
            (insert "")))))))

(defun llemacs--logging-clear-old-logs (days)
  "Remove log files older than DAYS."
  (let* ((current-time (current-time))
         (cutoff-time (time-subtract current-time (days-to-time days))))
    (dolist (file (directory-files llemacs--path-logging-logs t "\\.log"))
      (when (time-less-p
             (nth 5 (file-attributes file))
             cutoff-time)
        (delete-file file)))))

(defun llemacs--logging-compress-old-logs (days)
  "Compress log files older than DAYS."
  (let* ((current-time (current-time))
         (cutoff-time (time-subtract current-time (days-to-time days))))
    (dolist (file (directory-files llemacs--path-logging-logs t "\\.log$"))
      (when (and (time-less-p
                  (nth 5 (file-attributes file))
                  cutoff-time)
                 (not (string-match "\\.gz$" file)))
        (with-temp-buffer
          (insert-file-contents file)
          (write-region (point-min) (point-max) (concat file ".gz") nil nil nil 'gzip)
          (delete-file file))))))

(provide '02-llemacs-logging-file-maintainers)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))