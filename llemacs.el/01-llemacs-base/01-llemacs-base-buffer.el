;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:06:23
;;; Time-stamp: <2024-12-31 23:06:23 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base/02-llemacs-base-buffer.el

(require 'org)

(defcustom llemacs--buffer-main "*LLEMACS*"
  "Name of main buffer."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-log "*LLEMACS-LOGGING*"
  "Name of log buffer."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-debug "*LLEMACS-DEBUG*"
  "Buffer for debug output when debug mode enabled."
  :type 'string
  :group 'llemacs-buffer)

;; Feature-specific Buffers
(defcustom llemacs--buffer-project "*LLEMACS-PROJECT*"
  "Buffer for project operations."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-search "*LLEMACS-SEARCH*"
  "Buffer for search results."
  :type 'string
  :group 'llemacs-buffer)

;; Buffer initialization helper
(defun llemacs--buffer-init-org-buffer (buffer-name title &optional sections)
  "Initialize org buffer with BUFFER-NAME, TITLE and optional SECTIONS."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'org-mode)
        (org-mode)
        (insert (format "* %s\n" title))
        (when sections
          (dolist (section sections)
            (insert (format "** %s\n" section))))))
    buf))

;; Buffer getters
(defun llemacs--buffer-get-main-buffer ()
  "Initialize *LLEMACS* buffer if it doesn't exist."
  (let ((buf (llemacs--buffer-init-org-buffer
              llemacs--buffer-main
              "LLEMACS Main Buffer"
              '("Status" "Messages"))))
    (display-buffer buf)
    buf))

(defun llemacs--buffer-get-log-buffer ()
  "Initialize *LLEMACS-LOGGING* buffer if it doesn't exist."
  (let ((buf (llemacs--buffer-init-org-buffer
              llemacs--buffer-log
              "LLEMACS Logs"
              '("Recent Logs"))))
    (display-buffer buf)
    buf))

(defun llemacs--buffer-get-project-buffer ()
  "Get or create project buffer."
  (llemacs--buffer-init-org-buffer
   llemacs--buffer-project
   "Projects"))

(defun llemacs--buffer-get-search-buffer ()
  "Get or create search buffer."
  (llemacs--buffer-init-org-buffer
   llemacs--buffer-search
   "Search Results"))

(defun llemacs-buffer-get-debug-buffer ()
  "Get or create debug buffer."
  (llemacs--buffer-init-org-buffer
   llemacs--buffer-debug
   "Debug Information"))

(provide '02-llemacs-buffer)
(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-31 18:09:22
;; ;;; Time-stamp: <2024-12-31 18:09:22 (ywatanabe)>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-buffer.el

;; (require '01-llemacs-config)
;; (require 'org)


;; (defun llemacs--buffer-get-main ()
;;   "Initialize *LLEMACS* buffer if it doesn't exist."
;;   (let ((buf (get-buffer-create llemacs--buffer-main)))
;;     (with-current-buffer buf
;;       (unless (eq major-mode 'org-mode)
;;         (org-mode)
;;         (insert "* LLEMACS Main Buffer\n")
;;         (insert "** Status\n")
;;         (insert "** Messages\n")))
;;     (display-buffer buf)
;;     buf))

;; (defun llemacs--buffer-get-log-buffer ()
;;   "Initialize *LLEMACS-LOGGING* buffer if it doesn't exist."
;;   (let ((buf (get-buffer-create llemacs--buffer-log)))
;;     (with-current-buffer buf
;;       (unless (eq major-mode 'org-mode)
;;         (org-mode)
;;         (insert "* LLEMACS Logs\n")
;;         (insert "** Recent Logs\n")))
;;     (display-buffer buf)
;;     buf))

;; (defun llemacs--buffer-get-project-buffer ()
;;   "Get or create project buffer."
;;   (let ((buf (get-buffer-create llemacs--buffer-project)))
;;     (with-current-buffer buf
;;       (unless (eq major-mode 'org-mode)
;;         (org-mode)
;;         (insert "* Projects\n")))
;;     buf))

;; (defun llemacs--buffer-get-search-buffer ()
;;   "Get or create search buffer."
;;   (let ((buf (get-buffer-create llemacs--buffer-search)))
;;     (with-current-buffer buf
;;       (unless (eq major-mode 'org-mode)
;;         (org-mode)
;;         (insert "* Search Results\n")))
;;     buf))

;; (defun llemacs-buffer-get-debug-buffer ()
;;   "Get or create debug buffer."
;;   (let ((buf (get-buffer-create llemacs--buffer-debug)))
;;     (with-current-buffer buf
;;       (unless (eq major-mode 'org-mode)
;;         (org-mode)
;;         (insert "* Debug Information\n")))
;;     buf))

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;; Buffers
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (defun llemacs--buffer-get-main ()
;; ;;   "Initialize *LLEMACS* buffer if it doesn't exist."
;; ;;   (let ((buf (get-buffer-create llemacs--buffer-main)))
;; ;;     (with-current-buffer buf
;; ;;       (unless (eq major-mode 'org-mode)
;; ;;         (org-mode)))
;; ;;     (display-buffer buf)
;; ;;     buf))

;; ;; (defun llemacs--buffer-get-log-buffer ()
;; ;;   "Initialize *LLEMACS-LOGGING* buffer if it doesn't exist."  ;; Instead of "*LLEMACS*"
;; ;;   (let ((buf (get-buffer-create llemacs--buffer-log)))
;; ;;     (display-buffer buf)
;; ;;     buf))

;; ;; (defun llemacs--buffer-get-project-buffer ()
;; ;;   "Get or create project buffer."
;; ;;   (get-buffer-create llemacs--buffer-project))

;; ;; (defun llemacs--buffer-get-search-buffer ()
;; ;;   "Get or create search buffer."
;; ;;   (get-buffer-create llemacs--buffer-search))

;; ;; (defun llemacs-buffer-get-debug-buffer ()
;; ;;   "Get or create debug buffer."
;; ;;   (get-buffer-create llemacs--buffer-debug))

;; ;; ;; (llemacs--buffer-get-main)

;; ;; (defun llemacs-shell-command (command)
;; ;;   "Execute shell COMMAND and return output or nil on error."
;; ;;   (condition-case err
;; ;;       (with-temp-buffer
;; ;;         (let ((exit-code (call-process-shell-command command nil t)))
;; ;;           (if (zerop exit-code)
;; ;;               (buffer-string)
;; ;;             (error "Command failed with exit code %d: %s" exit-code command))))
;; ;;     (error (message "Shell command error: %s" err) nil)))

;; ;; (defun llemacs-diff-files (file1 file2)
;; ;;   "Get diff between FILE1 and FILE2."
;; ;;   (or (llemacs-shell-command (format "diff -u %s %s" file1 file2))
;; ;;       "No differences found"))


;; ;; (defun llemacs-ensure-workspace (dir-path)
;; ;;   "Ensure workspace directory exists and is accessible."
;; ;;   (unless (file-exists-p dir-path)
;; ;;     (make-directory dir-path t))
;; ;;   (cd dir-path))


;; ;; ;; (defun llemacs-show-progress (message)
;; ;; ;;   "Show progress MESSAGE in dedicated window."
;; ;; ;;   (let ((buffer (get-buffer-create "*llemacs-progress*")))
;; ;; ;;     (with-current-buffer buffer
;; ;; ;;       (goto-char (point-max))
;; ;; ;;       (let ((inhibit-read-only t))
;; ;; ;;         (insert (format "[%s] %s\n"
;; ;; ;;                         (format-time-string "%H:%M:%S")
;; ;; ;;                         message)))
;; ;; ;;       (display-buffer buffer))))

;; ;; ;; (defun llemacs-create-backup (file)
;; ;; ;;   "Create backup of FILE with timestamp."
;; ;; ;;   (when (and file (file-exists-p file))
;; ;; ;;     (let* ((base (file-name-sans-extension file))
;; ;; ;;            (ext (file-name-extension file))
;; ;; ;;            (timestamp (format-time-string "%Y%m%d-%H%M%S"))
;; ;; ;;            (backup-name (format "%s-%s.%s"
;; ;; ;;                                 (file-name-nondirectory base)
;; ;; ;;                                 timestamp
;; ;; ;;                                 ext))
;; ;; ;;            (backup-path (expand-file-name backup-name llemacs-backups-dir)))
;; ;; ;;       (condition-case err
;; ;; ;;           (progn
;; ;; ;;             (make-directory llemacs-backups-dir t)
;; ;; ;;             (copy-file file backup-path t)
;; ;; ;;             backup-path)
;; ;; ;;         (error (message "Backup failed for %s: %s" file err) nil)))))

;; ;; (defun llemacs-update-timestamp ()
;; ;;   "Update timestamp in file header."
;; ;;   (save-excursion
;; ;;     (goto-char (point-min))
;; ;;     (when (re-search-forward "Time-stamp: <.*>" nil t)
;; ;;       (let ((new-timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
;; ;;         (replace-match (format "Time-stamp: <%s (ywatanabe)>"
;; ;;                                new-timestamp))))))


;; (provide '02-llemacs-buffer)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))