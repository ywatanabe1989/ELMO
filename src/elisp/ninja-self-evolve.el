;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:56:43
;;; Time-stamp: <2024-12-04 08:56:43 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-self-evolve.el


(require 'ninja-config)
(require 'ninja-utils)
(require 'ninja-run)


;; (defun ninja-self-evolve (&optional file)
;;   "Update FILE with improvements suggested by LLM.
;; If FILE is nil, use ninja source directory."
;;   (interactive)
;;   (message "Starting self-evolution...")
;;   (ninja--init-workspace)
;;   (let* ((github-token (ninja--get-github-token))
;;          (file (or file (expand-file-name "ninja.el" ninja-source-dir)))
;;          (request-file ninja-user-request-file)
;;          (aspects (if (file-exists-p request-file)
;;                      (with-temp-buffer
;;                        (insert-file-contents request-file)
;;                        (buffer-string))
;;                    (read-string "Aspects to improve (empty for general review): ")))
;;          (workspace-dir ninja-workspace-dir)
;;          (ninja-load-path (file-name-directory (locate-library "ninja"))))

;;     (unless github-token
;;       (error "GitHub token not available. Check %s" ninja-github-token-file))

;;     (unless (file-exists-p file)
;;       (error "Source file not found: %s" file))

;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;;            (backup-file (ninja--create-backup work-file)))

;;       (copy-file file work-file t)

;;       (async-start
;;        `(lambda ()
;;           (add-to-list 'load-path ,ninja-load-path)
;;           (require 'ninja)
;;           (let ((default-directory ,default-directory))
;;             (ninja-run
;;              (format "Review and improve %s\nFocus on these aspects:\n%s"
;;                      ,work-file
;;                      ,(if (string-empty-p aspects)
;;                           "General code review and improvements"
;;                         aspects)))))
;;        `(lambda (_)
;;           (with-current-buffer (find-file-noselect ,work-file)
;;             (ninja--update-timestamp)
;;             (save-buffer))

;;           (when (file-exists-p ,backup-file)
;;             (let ((changes (ninja--diff-files ,backup-file ,work-file)))
;;               (ninja--log-change ,work-file ,backup-file changes)))

;;           (message "Self-evolution completed"))))))

;; ;; (defun ninja-self-evolve (&optional file)
;; ;;   "Update FILE with improvements suggested by LLM.
;; ;; If FILE is nil, use ninja source directory."
;; ;;   (interactive)
;; ;;   (ninja--init-workspace)
;; ;;   (let* ((github-token (ninja--get-github-token))
;; ;;          (file (or file (expand-file-name "ninja.el" ninja-source-dir)))
;; ;;          (request-file ninja-user-request-file)
;; ;;          (aspects (if (file-exists-p request-file)
;; ;;                      (with-temp-buffer
;; ;;                        (insert-file-contents request-file)
;; ;;                        (buffer-string))
;; ;;                    (read-string "Aspects to improve (empty for general review): ")))
;; ;;          (workspace-dir ninja-workspace-dir))

;; ;;     (unless github-token
;; ;;       (error "GitHub token not available. Check %s" ninja-github-token-file))

;; ;;     (unless (file-exists-p file)
;; ;;       (error "Source file not found: %s" file))

;; ;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;; ;;            (backup-file (ninja--create-backup work-file)))

;; ;;       (copy-file file work-file t)

;; ;;       (ninja-run
;; ;;        (format "Review and improve %s\nFocus on these aspects:\n%s"
;; ;;                work-file
;; ;;                (if (string-empty-p aspects)
;; ;;                    "General code review and improvements"
;; ;;                  aspects)))

;; ;;       (with-current-buffer (find-file-noselect work-file)
;; ;;         (ninja--update-timestamp)
;; ;;         (save-buffer))

;; ;;       (when (file-exists-p backup-file)
;; ;;         (let ((changes (ninja--diff-files backup-file work-file)))
;; ;;           (ninja--log-change work-file backup-file changes))))))
;; ; (ninja-self-evolve)

(provide 'ninja-self-evolve)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
