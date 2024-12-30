;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:36:02
;;; Time-stamp: <2024-12-29 08:36:02 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/04-llemacs-utils.el

(defun llemacs-get-main-buffer ()
  "Initialize *LLEMACS* buffer if it doesn't exist."
  (let ((buf (get-buffer-create llemacs--buffer-name-main)))
    (with-current-buffer buf
      (unless (eq major-mode 'org-mode)
        (org-mode)))
    (display-buffer buf)
    buf))

(defun llemacs-get-log-buffer ()
  "Initialize *LLEMACS* buffer if it doesn't exist."
  (let ((buf (get-buffer-create llemacs--buffer-name-log)))
    (display-buffer buf)
    buf))

;; (llemacs-get-main-buffer)

(defun llemacs-shell-command (command)
  "Execute shell COMMAND and return output or nil on error."
  (condition-case err
      (with-temp-buffer
        (let ((exit-code (call-process-shell-command command nil t)))
          (if (zerop exit-code)
              (buffer-string)
            (error "Command failed with exit code %d: %s" exit-code command))))
    (error (message "Shell command error: %s" err) nil)))

(defun llemacs-diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (or (llemacs-shell-command (format "diff -u %s %s" file1 file2))
      "No differences found"))


(defun llemacs-ensure-workspace (dir-path)
  "Ensure workspace directory exists and is accessible."
  (unless (file-exists-p dir-path)
    (make-directory dir-path t))
  (cd dir-path))


;; (defun llemacs-show-progress (message)
;;   "Show progress MESSAGE in dedicated window."
;;   (let ((buffer (get-buffer-create "*llemacs-progress*")))
;;     (with-current-buffer buffer
;;       (goto-char (point-max))
;;       (let ((inhibit-read-only t))
;;         (insert (format "[%s] %s\n"
;;                         (format-time-string "%H:%M:%S")
;;                         message)))
;;       (display-buffer buffer))))

;; (defun llemacs-create-backup (file)
;;   "Create backup of FILE with timestamp."
;;   (when (and file (file-exists-p file))
;;     (let* ((base (file-name-sans-extension file))
;;            (ext (file-name-extension file))
;;            (timestamp (format-time-string "%Y%m%d-%H%M%S"))
;;            (backup-name (format "%s-%s.%s"
;;                                 (file-name-nondirectory base)
;;                                 timestamp
;;                                 ext))
;;            (backup-path (expand-file-name backup-name llemacs-backups-dir)))
;;       (condition-case err
;;           (progn
;;             (make-directory llemacs-backups-dir t)
;;             (copy-file file backup-path t)
;;             backup-path)
;;         (error (message "Backup failed for %s: %s" file err) nil)))))

(defun llemacs-update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-llemacsrch-forward "Time-stamp: <.*>" nil t)
      (let ((new-timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
        (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                               new-timestamp))))))

(provide '04-llemacs-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))