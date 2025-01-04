;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:56:43
;;; Time-stamp: <2024-12-04 08:56:43 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-self-evolve.el


(require 'llemacs-config)
(require 'llemacs-utils)
(require 'llemacs-run)


;; (defun llemacs-self-evolve (&optional file)
;;   "Update FILE with improvements suggested by LLM.
;; If FILE is nil, use llemacs source directory."
;;   (interactive)
;;   (message "Starting self-evolution...")
;;   (llemacs-init-workspace)
;;   (let* ((github-token (llemacs-get-github-token))
;;          (file (or file (expand-file-name "llemacs.el" llemacs-source-dir)))
;;          (request-file llemacs-user-request-file)
;;          (aspects (if (file-exists-p request-file)
;;                      (with-temp-buffer
;;                        (insert-file-contents request-file)
;;                        (buffer-string))
;;                    (read-string "Aspects to improve (empty for general review): ")))
;;          (workspace-dir llemacs-workspace-dir)
;;          (llemacs-load-path (file-name-directory (locate-library "llemacs"))))

;;     (unless github-token
;;       (error "GitHub token not available. Check %s" llemacs-github-token-file))

;;     (unless (file-exists-p file)
;;       (error "Source file not found: %s" file))

;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;;            (backup-file (llemacs-create-backup work-file)))

;;       (copy-file file work-file t)

;;       (async-start
;;        `(lambda ()
;;           (add-to-list 'load-path ,llemacs-load-path)
;;           (require 'llemacs)
;;           (let ((default-directory ,default-directory))
;;             (llemacs-run
;;              (format "Review and improve %s\nFocus on these aspects:\n%s"
;;                      ,work-file
;;                      ,(if (string-empty-p aspects)
;;                           "General code review and improvements"
;;                         aspects)))))
;;        `(lambda (_)
;;           (with-current-buffer (find-file-noselect ,work-file)
;;             (llemacs-update-timestamp)
;;             (save-buffer))

;;           (when (file-exists-p ,backup-file)
;;             (let ((changes (llemacs-diff-files ,backup-file ,work-file)))
;;               (llemacs-log-change ,work-file ,backup-file changes)))

;;           (message "Self-evolution completed"))))))

;; ;; (defun llemacs-self-evolve (&optional file)
;; ;;   "Update FILE with improvements suggested by LLM.
;; ;; If FILE is nil, use llemacs source directory."
;; ;;   (interactive)
;; ;;   (llemacs-init-workspace)
;; ;;   (let* ((github-token (llemacs-get-github-token))
;; ;;          (file (or file (expand-file-name "llemacs.el" llemacs-source-dir)))
;; ;;          (request-file llemacs-user-request-file)
;; ;;          (aspects (if (file-exists-p request-file)
;; ;;                      (with-temp-buffer
;; ;;                        (insert-file-contents request-file)
;; ;;                        (buffer-string))
;; ;;                    (read-string "Aspects to improve (empty for general review): ")))
;; ;;          (workspace-dir llemacs-workspace-dir))

;; ;;     (unless github-token
;; ;;       (error "GitHub token not available. Check %s" llemacs-github-token-file))

;; ;;     (unless (file-exists-p file)
;; ;;       (error "Source file not found: %s" file))

;; ;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;; ;;            (backup-file (llemacs-create-backup work-file)))

;; ;;       (copy-file file work-file t)

;; ;;       (llemacs-run
;; ;;        (format "Review and improve %s\nFocus on these aspects:\n%s"
;; ;;                work-file
;; ;;                (if (string-empty-p aspects)
;; ;;                    "General code review and improvements"
;; ;;                  aspects)))

;; ;;       (with-current-buffer (find-file-noselect work-file)
;; ;;         (llemacs-update-timestamp)
;; ;;         (save-buffer))

;; ;;       (when (file-exists-p backup-file)
;; ;;         (let ((changes (llemacs-diff-files backup-file work-file)))
;; ;;           (llemacs-log-change work-file backup-file changes))))))
;; ; (llemacs-self-evolve)

(provide 'llemacs-self-evolve)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
