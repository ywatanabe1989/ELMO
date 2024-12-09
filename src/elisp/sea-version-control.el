;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:58:01
;;; Time-stamp: <2024-12-04 08:58:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'sea-utils)
(require 'sea-prompts)

(defgroup sea-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'sea)

(defcustom sea-git-user-name "sea"
  "Git user name for SEA commits."
  :type 'string
  :group 'sea-git)

(defcustom sea-git-user-email "sea@example.com"
  "Git email for SEA commits."
  :type 'string
  :group 'sea-git)

(defcustom sea-github-token-file "~/.config/sea/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'sea-git)

(defvar sea--github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

;; (defun sea--validate-github-token (token)
;;   "Validate TOKEN format and basic structure."
;;   (when (or (null token)
;;             (not (stringp token))
;;             (string-empty-p token)
;;             (< (length token) 40))
;;     (error "Invalid GitHub token format")))

(defun sea--validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (sea--log-message "Invalid GitHub token format")
    (error "Invalid GitHub token format")))

;; (defun sea--load-github-token ()
;;   "Load GitHub token from file with validation and error handling."
;;   (condition-case err
;;       (let* ((token-file (expand-file-name sea-github-token-file))
;;              (real-file (file-truename token-file)))
;;         (unless (file-exists-p real-file)
;;           (error "GitHub token file not found: %s" real-file))
;;         (unless (file-readable-p real-file)
;;           (error "GitHub token file not readable: %s" real-file))
;;         (let ((token (with-temp-buffer
;;                       (insert-file-contents real-file)
;;                       (string-trim (buffer-string)))))
;;           (sea--validate-github-token token)
;;           token))
;;     (error
;;      (message "Failed to load GitHub token: %s" (error-message-string err))
;;      nil)))

(defun sea--load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name sea-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (sea--log-message (format "GitHub token file not found: %s" real-file))
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (sea--log-message (format "GitHub token file not readable: %s" real-file))
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (sea--validate-github-token token)
          token))
    (error
     (sea--log-message (format "Failed to load GitHub token: %s" err))
     nil)))

(defun sea--get-github-token ()
  "Get GitHub token, using cache if available."
  (or sea--github-token-cache
      (setq sea--github-token-cache (sea--load-github-token))))

;; Replace existing initialization with:
(sea--get-github-token)

;; (defun sea--ensure-not-main ()
;;   "Ensure we're not on main branch."
;;   (let ((current-branch
;;          (string-trim
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
;;     (when (string= current-branch "main")
;;       (error "Cannot modify main branch directly"))))
(defun sea--ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (sea--log-message "Cannot modify main branch directly")
      (error "Cannot modify main branch directly"))))

;; (defun sea-commit-changes (file msg)
;;   "Commit changes to FILE with commit MSG."
;;   (sea--ensure-not-main)
;;   (let ((default-directory (file-name-directory file)))
;;     (sea--shell-command
;;      (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
;;              sea-git-user-name
;;              sea-git-user-email
;;              (file-name-nondirectory file)
;;              sea-git-user-name
;;              sea-git-user-email
;;              msg))))
(defun sea-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (condition-case err
      (progn
        (sea--ensure-not-main)
        (let ((default-directory (file-name-directory file)))
          (sea--shell-command
           (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
                   sea-git-user-name sea-git-user-email
                   (file-name-nondirectory file)
                   sea-git-user-name sea-git-user-email msg))
          (sea--log-message (format "Committed changes to %s" file))))
    (error
     (sea--log-message (format "Failed to commit changes: %s" err))
     nil)))

;; (defun sea-push-changes ()
;;   "Push changes to sea-develop branch."
;;   (sea--ensure-not-main)
;;   (sea--shell-command
;;    (concat
;;     "git checkout -b sea-develop 2>/dev/null || git checkout sea-develop && "
;;     "git push -u origin sea-develop")))
(defun sea-push-changes ()
  "Push changes to sea-develop branch."
  (condition-case err
      (progn
        (sea--ensure-not-main)
        (sea--shell-command
         (concat "git checkout -b sea-develop 2>/dev/null || git checkout sea-develop && "
                "git push -u origin sea-develop"))
        (sea--log-message "Pushed changes to sea-develop branch"))
    (error
     (sea--log-message (format "Failed to push changes: %s" err))
     nil)))

;; (defun sea-create-pr (title body)
;;   "Create pull request with TITLE and BODY from sea-develop to main."
;;   (let ((token (sea--get-github-token)))
;;     (unless token
;;       (error "GitHub token not available"))
;;     (let ((url "https://api.github.com/repos/owner/repo/pulls")
;;           (headers `(("Authorization" . ,(concat "token " token))
;;                     ("Accept" . "application/vnd.github.v3+json")))
;;           (data (json-encode
;;                  `((title . ,title)
;;                    (body . ,body)
;;                    (head . "sea-develop")
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
;;          (setq sea--github-token-cache nil)
;;          (error "Failed to create PR: %s" (error-message-string err)))))))

(defun sea-create-pr (title body)
  "Create pull request with TITLE and BODY from sea-develop to main."
  (condition-case err
      (let ((token (sea--get-github-token)))
        (unless token
          (sea--log-message "GitHub token not available")
          (error "GitHub token not available"))
        (request "https://api.github.com/repos/owner/repo/pulls"
                 :type "POST"
                 :headers `(("Authorization" . ,(concat "token " token))
                          ("Accept" . "application/vnd.github.v3+json"))
                 :data (json-encode
                       `((title . ,title)
                         (body . ,body)
                         (head . "sea-develop")
                         (base . "main")))
                 :parser 'json-read
                 :success (lambda (&rest _)
                           (sea--log-message "Pull request created successfully"))
                 :error (lambda (&rest args)
                         (sea--log-message (format "PR creation failed: %S" args))
                         (error "PR creation failed: %S" args))))
    (error
     (sea--log-message (format "Failed to create PR: %s" err))
     (setq sea--github-token-cache nil)
     nil)))

(provide 'sea-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
