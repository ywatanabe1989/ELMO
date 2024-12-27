;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 08:20:12
;;; Time-stamp: <2024-12-27 08:20:12 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/04-elmo-utils.el

(defun elmo-get-main-buffer ()
  "Initialize *ELMO* buffer if it doesn't exist."
  (let ((buf (get-buffer-create elmo-main-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'org-mode)
        (org-mode)))
    (display-buffer buf)
    buf))

(defun elmo-get-log-buffer ()
  "Initialize *ELMO* buffer if it doesn't exist."
  (let ((buf (get-buffer-create elmo-log-buffer-name)))
    (display-buffer buf)
    buf))

;; (elmo-get-main-buffer)

(defun elmo-shell-command (command)
  "Execute shell COMMAND and return output or nil on error."
  (condition-case err
      (with-temp-buffer
        (let ((exit-code (call-process-shell-command command nil t)))
          (if (zerop exit-code)
              (buffer-string)
            (error "Command failed with exit code %d: %s" exit-code command))))
    (error (message "Shell command error: %s" err) nil)))

(defun elmo-diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (or (elmo-shell-command (format "diff -u %s %s" file1 file2))
      "No differences found"))


(defun elmo-ensure-workspace (dir-path)
  "Ensure workspace directory exists and is accessible."
  (unless (file-exists-p dir-path)
    (make-directory dir-path t))
  (cd dir-path))


;; (defun elmo-show-progress (message)
;;   "Show progress MESSAGE in dedicated window."
;;   (let ((buffer (get-buffer-create "*elmo-progress*")))
;;     (with-current-buffer buffer
;;       (goto-char (point-max))
;;       (let ((inhibit-read-only t))
;;         (insert (format "[%s] %s\n"
;;                         (format-time-string "%H:%M:%S")
;;                         message)))
;;       (display-buffer buffer))))

;; (defun elmo-create-backup (file)
;;   "Create backup of FILE with timestamp."
;;   (when (and file (file-exists-p file))
;;     (let* ((base (file-name-sans-extension file))
;;            (ext (file-name-extension file))
;;            (timestamp (format-time-string "%Y%m%d-%H%M%S"))
;;            (backup-name (format "%s-%s.%s"
;;                                 (file-name-nondirectory base)
;;                                 timestamp
;;                                 ext))
;;            (backup-path (expand-file-name backup-name elmo-backups-dir)))
;;       (condition-case err
;;           (progn
;;             (make-directory elmo-backups-dir t)
;;             (copy-file file backup-path t)
;;             backup-path)
;;         (error (message "Backup failed for %s: %s" file err) nil)))))

(defun elmo-update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-elmorch-forward "Time-stamp: <.*>" nil t)
      (let ((new-timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
        (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                               new-timestamp))))))

(provide '04-elmo-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))