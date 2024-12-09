;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:58:01
;;; Time-stamp: <2024-12-04 08:58:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'semacs-utils)
(require 'semacs-prompts)

(defgroup semacs-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'semacs)

(defcustom semacs-git-user-name "semacs"
  "Git user name for SEMACS commits."
  :type 'string
  :group 'semacs-git)

(defcustom semacs-git-user-email "semacs@example.com"
  "Git email for SEMACS commits."
  :type 'string
  :group 'semacs-git)

(defcustom semacs-github-token-file "~/.config/semacs/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'semacs-git)

(defvar semacs--github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

;; (defun semacs--validate-github-token (token)
;;   "Validate TOKEN format and basic structure."
;;   (when (or (null token)
;;             (not (stringp token))
;;             (string-empty-p token)
;;             (< (length token) 40))
;;     (error "Invalid GitHub token format")))

(defun semacs--validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (semacs--log-message "Invalid GitHub token format")
    (error "Invalid GitHub token format")))

;; (defun semacs--load-github-token ()
;;   "Load GitHub token from file with validation and error handling."
;;   (condition-case err
;;       (let* ((token-file (expand-file-name semacs-github-token-file))
;;              (real-file (file-truename token-file)))
;;         (unless (file-exists-p real-file)
;;           (error "GitHub token file not found: %s" real-file))
;;         (unless (file-readable-p real-file)
;;           (error "GitHub token file not readable: %s" real-file))
;;         (let ((token (with-temp-buffer
;;                       (insert-file-contents real-file)
;;                       (string-trim (buffer-string)))))
;;           (semacs--validate-github-token token)
;;           token))
;;     (error
;;      (message "Failed to load GitHub token: %s" (error-message-string err))
;;      nil)))

(defun semacs--load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name semacs-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (semacs--log-message (format "GitHub token file not found: %s" real-file))
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (semacs--log-message (format "GitHub token file not readable: %s" real-file))
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (semacs--validate-github-token token)
          token))
    (error
     (semacs--log-message (format "Failed to load GitHub token: %s" err))
     nil)))

(defun semacs--get-github-token ()
  "Get GitHub token, using cache if available."
  (or semacs--github-token-cache
      (setq semacs--github-token-cache (semacs--load-github-token))))

;; Replace existing initialization with:
(semacs--get-github-token)

;; (defun semacs--ensure-not-main ()
;;   "Ensure we're not on main branch."
;;   (let ((current-branch
;;          (string-trim
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
;;     (when (string= current-branch "main")
;;       (error "Cannot modify main branch directly"))))
(defun semacs--ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (semacs--log-message "Cannot modify main branch directly")
      (error "Cannot modify main branch directly"))))

;; (defun semacs-commit-changes (file msg)
;;   "Commit changes to FILE with commit MSG."
;;   (semacs--ensure-not-main)
;;   (let ((default-directory (file-name-directory file)))
;;     (semacs--shell-command
;;      (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
;;              semacs-git-user-name
;;              semacs-git-user-email
;;              (file-name-nondirectory file)
;;              semacs-git-user-name
;;              semacs-git-user-email
;;              msg))))
(defun semacs-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (condition-case err
      (progn
        (semacs--ensure-not-main)
        (let ((default-directory (file-name-directory file)))
          (semacs--shell-command
           (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
                   semacs-git-user-name semacs-git-user-email
                   (file-name-nondirectory file)
                   semacs-git-user-name semacs-git-user-email msg))
          (semacs--log-message (format "Committed changes to %s" file))))
    (error
     (semacs--log-message (format "Failed to commit changes: %s" err))
     nil)))

;; (defun semacs-push-changes ()
;;   "Push changes to semacs-develop branch."
;;   (semacs--ensure-not-main)
;;   (semacs--shell-command
;;    (concat
;;     "git checkout -b semacs-develop 2>/dev/null || git checkout semacs-develop && "
;;     "git push -u origin semacs-develop")))
(defun semacs-push-changes ()
  "Push changes to semacs-develop branch."
  (condition-case err
      (progn
        (semacs--ensure-not-main)
        (semacs--shell-command
         (concat "git checkout -b semacs-develop 2>/dev/null || git checkout semacs-develop && "
                "git push -u origin semacs-develop"))
        (semacs--log-message "Pushed changes to semacs-develop branch"))
    (error
     (semacs--log-message (format "Failed to push changes: %s" err))
     nil)))

;; (defun semacs-create-pr (title body)
;;   "Create pull request with TITLE and BODY from semacs-develop to main."
;;   (let ((token (semacs--get-github-token)))
;;     (unless token
;;       (error "GitHub token not available"))
;;     (let ((url "https://api.github.com/repos/owner/repo/pulls")
;;           (headers `(("Authorization" . ,(concat "token " token))
;;                     ("Accept" . "application/vnd.github.v3+json")))
;;           (data (json-encode
;;                  `((title . ,title)
;;                    (body . ,body)
;;                    (head . "semacs-develop")
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
;;          (setq semacs--github-token-cache nil)
;;          (error "Failed to create PR: %s" (error-message-string err)))))))

(defun semacs-create-pr (title body)
  "Create pull request with TITLE and BODY from semacs-develop to main."
  (condition-case err
      (let ((token (semacs--get-github-token)))
        (unless token
          (semacs--log-message "GitHub token not available")
          (error "GitHub token not available"))
        (request "https://api.github.com/repos/owner/repo/pulls"
                 :type "POST"
                 :headers `(("Authorization" . ,(concat "token " token))
                          ("Accept" . "application/vnd.github.v3+json"))
                 :data (json-encode
                       `((title . ,title)
                         (body . ,body)
                         (head . "semacs-develop")
                         (base . "main")))
                 :parser 'json-read
                 :success (lambda (&rest _)
                           (semacs--log-message "Pull request created successfully"))
                 :error (lambda (&rest args)
                         (semacs--log-message (format "PR creation failed: %S" args))
                         (error "PR creation failed: %S" args))))
    (error
     (semacs--log-message (format "Failed to create PR: %s" err))
     (setq semacs--github-token-cache nil)
     nil)))

(provide 'semacs-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
