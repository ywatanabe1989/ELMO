;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:56:43
;;; Time-stamp: <2024-12-04 08:56:43 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-self-evolve.el


(require 'elmo-config)
(require 'elmo-utils)
(require 'elmo-run)


;; (defun elmo-self-evolve (&optional file)
;;   "Update FILE with improvements suggested by LLM.
;; If FILE is nil, use elmo source directory."
;;   (interactive)
;;   (message "Starting self-evolution...")
;;   (elmo-init-workspace)
;;   (let* ((github-token (elmo-get-github-token))
;;          (file (or file (expand-file-name "elmo.el" elmo-source-dir)))
;;          (request-file elmo-user-request-file)
;;          (aspects (if (file-exists-p request-file)
;;                      (with-temp-buffer
;;                        (insert-file-contents request-file)
;;                        (buffer-string))
;;                    (read-string "Aspects to improve (empty for general review): ")))
;;          (workspace-dir elmo-workspace-dir)
;;          (elmo-load-path (file-name-directory (locate-library "elmo"))))

;;     (unless github-token
;;       (error "GitHub token not available. Check %s" elmo-github-token-file))

;;     (unless (file-exists-p file)
;;       (error "Source file not found: %s" file))

;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;;            (backup-file (elmo-create-backup work-file)))

;;       (copy-file file work-file t)

;;       (async-start
;;        `(lambda ()
;;           (add-to-list 'load-path ,elmo-load-path)
;;           (require 'elmo)
;;           (let ((default-directory ,default-directory))
;;             (elmo-run
;;              (format "Review and improve %s\nFocus on these aspects:\n%s"
;;                      ,work-file
;;                      ,(if (string-empty-p aspects)
;;                           "General code review and improvements"
;;                         aspects)))))
;;        `(lambda (_)
;;           (with-current-buffer (find-file-noselect ,work-file)
;;             (elmo-update-timestamp)
;;             (save-buffer))

;;           (when (file-exists-p ,backup-file)
;;             (let ((changes (elmo-diff-files ,backup-file ,work-file)))
;;               (elmo-log-change ,work-file ,backup-file changes)))

;;           (message "Self-evolution completed"))))))

;; ;; (defun elmo-self-evolve (&optional file)
;; ;;   "Update FILE with improvements suggested by LLM.
;; ;; If FILE is nil, use elmo source directory."
;; ;;   (interactive)
;; ;;   (elmo-init-workspace)
;; ;;   (let* ((github-token (elmo-get-github-token))
;; ;;          (file (or file (expand-file-name "elmo.el" elmo-source-dir)))
;; ;;          (request-file elmo-user-request-file)
;; ;;          (aspects (if (file-exists-p request-file)
;; ;;                      (with-temp-buffer
;; ;;                        (insert-file-contents request-file)
;; ;;                        (buffer-string))
;; ;;                    (read-string "Aspects to improve (empty for general review): ")))
;; ;;          (workspace-dir elmo-workspace-dir))

;; ;;     (unless github-token
;; ;;       (error "GitHub token not available. Check %s" elmo-github-token-file))

;; ;;     (unless (file-exists-p file)
;; ;;       (error "Source file not found: %s" file))

;; ;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;; ;;            (backup-file (elmo-create-backup work-file)))

;; ;;       (copy-file file work-file t)

;; ;;       (elmo-run
;; ;;        (format "Review and improve %s\nFocus on these aspects:\n%s"
;; ;;                work-file
;; ;;                (if (string-empty-p aspects)
;; ;;                    "General code review and improvements"
;; ;;                  aspects)))

;; ;;       (with-current-buffer (find-file-noselect work-file)
;; ;;         (elmo-update-timestamp)
;; ;;         (save-buffer))

;; ;;       (when (file-exists-p backup-file)
;; ;;         (let ((changes (elmo-diff-files backup-file work-file)))
;; ;;           (elmo-log-change work-file backup-file changes))))))
;; ; (elmo-self-evolve)

(provide 'elmo-self-evolve)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
