;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:56:43
;;; Time-stamp: <2024-12-04 08:56:43 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-self-evolve.el


(require 'semacs-config)
(require 'semacs-utils)
(require 'semacs-run)


;; (defun semacs-self-evolve (&optional file)
;;   "Update FILE with improvements suggested by LLM.
;; If FILE is nil, use semacs source directory."
;;   (interactive)
;;   (message "Starting self-evolution...")
;;   (semacs--init-workspace)
;;   (let* ((github-token (semacs--get-github-token))
;;          (file (or file (expand-file-name "semacs.el" semacs-source-dir)))
;;          (request-file semacs-user-request-file)
;;          (aspects (if (file-exists-p request-file)
;;                      (with-temp-buffer
;;                        (insert-file-contents request-file)
;;                        (buffer-string))
;;                    (read-string "Aspects to improve (empty for general review): ")))
;;          (workspace-dir semacs-workspace-dir)
;;          (semacs-load-path (file-name-directory (locate-library "semacs"))))

;;     (unless github-token
;;       (error "GitHub token not available. Check %s" semacs-github-token-file))

;;     (unless (file-exists-p file)
;;       (error "Source file not found: %s" file))

;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;;            (backup-file (semacs--create-backup work-file)))

;;       (copy-file file work-file t)

;;       (async-start
;;        `(lambda ()
;;           (add-to-list 'load-path ,semacs-load-path)
;;           (require 'semacs)
;;           (let ((default-directory ,default-directory))
;;             (semacs-run
;;              (format "Review and improve %s\nFocus on these aspects:\n%s"
;;                      ,work-file
;;                      ,(if (string-empty-p aspects)
;;                           "General code review and improvements"
;;                         aspects)))))
;;        `(lambda (_)
;;           (with-current-buffer (find-file-noselect ,work-file)
;;             (semacs--update-timestamp)
;;             (save-buffer))

;;           (when (file-exists-p ,backup-file)
;;             (let ((changes (semacs--diff-files ,backup-file ,work-file)))
;;               (semacs--log-change ,work-file ,backup-file changes)))

;;           (message "Self-evolution completed"))))))

;; ;; (defun semacs-self-evolve (&optional file)
;; ;;   "Update FILE with improvements suggested by LLM.
;; ;; If FILE is nil, use semacs source directory."
;; ;;   (interactive)
;; ;;   (semacs--init-workspace)
;; ;;   (let* ((github-token (semacs--get-github-token))
;; ;;          (file (or file (expand-file-name "semacs.el" semacs-source-dir)))
;; ;;          (request-file semacs-user-request-file)
;; ;;          (aspects (if (file-exists-p request-file)
;; ;;                      (with-temp-buffer
;; ;;                        (insert-file-contents request-file)
;; ;;                        (buffer-string))
;; ;;                    (read-string "Aspects to improve (empty for general review): ")))
;; ;;          (workspace-dir semacs-workspace-dir))

;; ;;     (unless github-token
;; ;;       (error "GitHub token not available. Check %s" semacs-github-token-file))

;; ;;     (unless (file-exists-p file)
;; ;;       (error "Source file not found: %s" file))

;; ;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;; ;;            (backup-file (semacs--create-backup work-file)))

;; ;;       (copy-file file work-file t)

;; ;;       (semacs-run
;; ;;        (format "Review and improve %s\nFocus on these aspects:\n%s"
;; ;;                work-file
;; ;;                (if (string-empty-p aspects)
;; ;;                    "General code review and improvements"
;; ;;                  aspects)))

;; ;;       (with-current-buffer (find-file-noselect work-file)
;; ;;         (semacs--update-timestamp)
;; ;;         (save-buffer))

;; ;;       (when (file-exists-p backup-file)
;; ;;         (let ((changes (semacs--diff-files backup-file work-file)))
;; ;;           (semacs--log-change work-file backup-file changes))))))
;; ; (semacs-self-evolve)

(provide 'semacs-self-evolve)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
