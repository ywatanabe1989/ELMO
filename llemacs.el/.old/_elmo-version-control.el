;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:58:01
;;; Time-stamp: <2024-12-04 08:58:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'llemacs-utils)
(require 'llemacs-prompts)

(defgroup llemacs-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'llemacs)

(defcustom llemacs-git-user-name "llemacs"
  "Git user name for ELMO commits."
  :type 'string
  :group 'llemacs-git)

(defcustom llemacs-git-user-email "llemacs@example.com"
  "Git email for ELMO commits."
  :type 'string
  :group 'llemacs-git)

(defcustom llemacs-github-token-file "~/.config/llemacs/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'llemacs-git)

(defvar llemacs-github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

;; (defun llemacs-validate-github-token (token)
;;   "Validate TOKEN format and basic structure."
;;   (when (or (null token)
;;             (not (stringp token))
;;             (string-empty-p token)
;;             (< (length token) 40))
;;     (error "Invalid GitHub token format")))

(defun llemacs-validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (llemacs-log-message "Invalid GitHub token format")
    (error "Invalid GitHub token format")))

;; (defun llemacs-load-github-token ()
;;   "Load GitHub token from file with validation and error handling."
;;   (condition-case err
;;       (let* ((token-file (expand-file-name llemacs-github-token-file))
;;              (real-file (file-truename token-file)))
;;         (unless (file-exists-p real-file)
;;           (error "GitHub token file not found: %s" real-file))
;;         (unless (file-readable-p real-file)
;;           (error "GitHub token file not readable: %s" real-file))
;;         (let ((token (with-temp-buffer
;;                       (insert-file-contents real-file)
;;                       (string-trim (buffer-string)))))
;;           (llemacs-validate-github-token token)
;;           token))
;;     (error
;;      (message "Failed to load GitHub token: %s" (error-message-string err))
;;      nil)))

(defun llemacs-load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name llemacs-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (llemacs-log-message (format "GitHub token file not found: %s" real-file))
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (llemacs-log-message (format "GitHub token file not readable: %s" real-file))
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (llemacs-validate-github-token token)
          token))
    (error
     (llemacs-log-message (format "Failed to load GitHub token: %s" err))
     nil)))

(defun llemacs-get-github-token ()
  "Get GitHub token, using cache if available."
  (or llemacs-github-token-cache
      (setq llemacs-github-token-cache (llemacs-load-github-token))))

;; Replace existing initialization with:
(llemacs-get-github-token)

;; (defun llemacs-ensure-not-main ()
;;   "Ensure we're not on main branch."
;;   (let ((current-branch
;;          (string-trim
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
;;     (when (string= current-branch "main")
;;       (error "Cannot modify main branch directly"))))
(defun llemacs-ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (llemacs-log-message "Cannot modify main branch directly")
      (error "Cannot modify main branch directly"))))

;; (defun llemacs-commit-changes (file msg)
;;   "Commit changes to FILE with commit MSG."
;;   (llemacs-ensure-not-main)
;;   (let ((default-directory (file-name-directory file)))
;;     (llemacs-shell-command
;;      (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
;;              llemacs-git-user-name
;;              llemacs-git-user-email
;;              (file-name-nondirectory file)
;;              llemacs-git-user-name
;;              llemacs-git-user-email
;;              msg))))
(defun llemacs-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (condition-case err
      (progn
        (llemacs-ensure-not-main)
        (let ((default-directory (file-name-directory file)))
          (llemacs-shell-command
           (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
                   llemacs-git-user-name llemacs-git-user-email
                   (file-name-nondirectory file)
                   llemacs-git-user-name llemacs-git-user-email msg))
          (llemacs-log-message (format "Committed changes to %s" file))))
    (error
     (llemacs-log-message (format "Failed to commit changes: %s" err))
     nil)))

;; (defun llemacs-push-changes ()
;;   "Push changes to llemacs-develop branch."
;;   (llemacs-ensure-not-main)
;;   (llemacs-shell-command
;;    (concat
;;     "git checkout -b llemacs-develop 2>/dev/null || git checkout llemacs-develop && "
;;     "git push -u origin llemacs-develop")))
(defun llemacs-push-changes ()
  "Push changes to llemacs-develop branch."
  (condition-case err
      (progn
        (llemacs-ensure-not-main)
        (llemacs-shell-command
         (concat "git checkout -b llemacs-develop 2>/dev/null || git checkout llemacs-develop && "
                "git push -u origin llemacs-develop"))
        (llemacs-log-message "Pushed changes to llemacs-develop branch"))
    (error
     (llemacs-log-message (format "Failed to push changes: %s" err))
     nil)))

;; (defun llemacs-create-pr (title body)
;;   "Create pull request with TITLE and BODY from llemacs-develop to main."
;;   (let ((token (llemacs-get-github-token)))
;;     (unless token
;;       (error "GitHub token not available"))
;;     (let ((url "https://api.github.com/repos/owner/repo/pulls")
;;           (headers `(("Authorization" . ,(concat "token " token))
;;                     ("Accept" . "application/vnd.github.v3+json")))
;;           (data (json-encode
;;                  `((title . ,title)
;;                    (body . ,body)
;;                    (head . "llemacs-develop")
;;                    (base . "main")))))
;;       (condition-case err
;;           (request url
;;                    :type "POST"
;;                    :headers headers
;;                    :data data
;;                    :parser 'json-read
;;                    :error (lambda (&rest args)
;;                            (error "PR creation failed: %S" args)))
;;         (error
;;          (setq llemacs-github-token-cache nil)
;;          (error "Failed to create PR: %s" (error-message-string err)))))))

(defun llemacs-create-pr (title body)
  "Create pull request with TITLE and BODY from llemacs-develop to main."
  (condition-case err
      (let ((token (llemacs-get-github-token)))
        (unless token
          (llemacs-log-message "GitHub token not available")
          (error "GitHub token not available"))
        (request "https://api.github.com/repos/owner/repo/pulls"
                 :type "POST"
                 :headers `(("Authorization" . ,(concat "token " token))
                          ("Accept" . "application/vnd.github.v3+json"))
                 :data (json-encode
                       `((title . ,title)
                         (body . ,body)
                         (head . "llemacs-develop")
                         (base . "main")))
                 :parser 'json-read
                 :success (lambda (&rest _)
                           (llemacs-log-message "Pull request created successfully"))
                 :error (lambda (&rest args)
                         (llemacs-log-message (format "PR creation failed: %S" args))
                         (error "PR creation failed: %S" args))))
    (error
     (llemacs-log-message (format "Failed to create PR: %s" err))
     (setq llemacs-github-token-cache nil)
     nil)))

(provide 'llemacs-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
