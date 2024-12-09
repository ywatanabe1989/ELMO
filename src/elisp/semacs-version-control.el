;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:58:01
;;; Time-stamp: <2024-12-04 08:58:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'ninja-utils)
(require 'ninja-prompts)

(defgroup ninja-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'ninja)

(defcustom ninja-git-user-name "ninja"
  "Git user name for NINJA commits."
  :type 'string
  :group 'ninja-git)

(defcustom ninja-git-user-email "ninja@example.com"
  "Git email for NINJA commits."
  :type 'string
  :group 'ninja-git)

(defcustom ninja-github-token-file "~/.config/ninja/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'ninja-git)

(defvar ninja--github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

;; (defun ninja--validate-github-token (token)
;;   "Validate TOKEN format and basic structure."
;;   (when (or (null token)
;;             (not (stringp token))
;;             (string-empty-p token)
;;             (< (length token) 40))
;;     (error "Invalid GitHub token format")))

(defun ninja--validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (ninja--log-message "Invalid GitHub token format")
    (error "Invalid GitHub token format")))

;; (defun ninja--load-github-token ()
;;   "Load GitHub token from file with validation and error handling."
;;   (condition-case err
;;       (let* ((token-file (expand-file-name ninja-github-token-file))
;;              (real-file (file-truename token-file)))
;;         (unless (file-exists-p real-file)
;;           (error "GitHub token file not found: %s" real-file))
;;         (unless (file-readable-p real-file)
;;           (error "GitHub token file not readable: %s" real-file))
;;         (let ((token (with-temp-buffer
;;                       (insert-file-contents real-file)
;;                       (string-trim (buffer-string)))))
;;           (ninja--validate-github-token token)
;;           token))
;;     (error
;;      (message "Failed to load GitHub token: %s" (error-message-string err))
;;      nil)))

(defun ninja--load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name ninja-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (ninja--log-message (format "GitHub token file not found: %s" real-file))
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (ninja--log-message (format "GitHub token file not readable: %s" real-file))
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (ninja--validate-github-token token)
          token))
    (error
     (ninja--log-message (format "Failed to load GitHub token: %s" err))
     nil)))

(defun ninja--get-github-token ()
  "Get GitHub token, using cache if available."
  (or ninja--github-token-cache
      (setq ninja--github-token-cache (ninja--load-github-token))))

;; Replace existing initialization with:
(ninja--get-github-token)

;; (defun ninja--ensure-not-main ()
;;   "Ensure we're not on main branch."
;;   (let ((current-branch
;;          (string-trim
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
;;     (when (string= current-branch "main")
;;       (error "Cannot modify main branch directly"))))
(defun ninja--ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (ninja--log-message "Cannot modify main branch directly")
      (error "Cannot modify main branch directly"))))

;; (defun ninja-commit-changes (file msg)
;;   "Commit changes to FILE with commit MSG."
;;   (ninja--ensure-not-main)
;;   (let ((default-directory (file-name-directory file)))
;;     (ninja--shell-command
;;      (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
;;              ninja-git-user-name
;;              ninja-git-user-email
;;              (file-name-nondirectory file)
;;              ninja-git-user-name
;;              ninja-git-user-email
;;              msg))))
(defun ninja-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (condition-case err
      (progn
        (ninja--ensure-not-main)
        (let ((default-directory (file-name-directory file)))
          (ninja--shell-command
           (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
                   ninja-git-user-name ninja-git-user-email
                   (file-name-nondirectory file)
                   ninja-git-user-name ninja-git-user-email msg))
          (ninja--log-message (format "Committed changes to %s" file))))
    (error
     (ninja--log-message (format "Failed to commit changes: %s" err))
     nil)))

;; (defun ninja-push-changes ()
;;   "Push changes to ninja-develop branch."
;;   (ninja--ensure-not-main)
;;   (ninja--shell-command
;;    (concat
;;     "git checkout -b ninja-develop 2>/dev/null || git checkout ninja-develop && "
;;     "git push -u origin ninja-develop")))
(defun ninja-push-changes ()
  "Push changes to ninja-develop branch."
  (condition-case err
      (progn
        (ninja--ensure-not-main)
        (ninja--shell-command
         (concat "git checkout -b ninja-develop 2>/dev/null || git checkout ninja-develop && "
                "git push -u origin ninja-develop"))
        (ninja--log-message "Pushed changes to ninja-develop branch"))
    (error
     (ninja--log-message (format "Failed to push changes: %s" err))
     nil)))

;; (defun ninja-create-pr (title body)
;;   "Create pull request with TITLE and BODY from ninja-develop to main."
;;   (let ((token (ninja--get-github-token)))
;;     (unless token
;;       (error "GitHub token not available"))
;;     (let ((url "https://api.github.com/repos/owner/repo/pulls")
;;           (headers `(("Authorization" . ,(concat "token " token))
;;                     ("Accept" . "application/vnd.github.v3+json")))
;;           (data (json-encode
;;                  `((title . ,title)
;;                    (body . ,body)
;;                    (head . "ninja-develop")
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
;;          (setq ninja--github-token-cache nil)
;;          (error "Failed to create PR: %s" (error-message-string err)))))))

(defun ninja-create-pr (title body)
  "Create pull request with TITLE and BODY from ninja-develop to main."
  (condition-case err
      (let ((token (ninja--get-github-token)))
        (unless token
          (ninja--log-message "GitHub token not available")
          (error "GitHub token not available"))
        (request "https://api.github.com/repos/owner/repo/pulls"
                 :type "POST"
                 :headers `(("Authorization" . ,(concat "token " token))
                          ("Accept" . "application/vnd.github.v3+json"))
                 :data (json-encode
                       `((title . ,title)
                         (body . ,body)
                         (head . "ninja-develop")
                         (base . "main")))
                 :parser 'json-read
                 :success (lambda (&rest _)
                           (ninja--log-message "Pull request created successfully"))
                 :error (lambda (&rest args)
                         (ninja--log-message (format "PR creation failed: %S" args))
                         (error "PR creation failed: %S" args))))
    (error
     (ninja--log-message (format "Failed to create PR: %s" err))
     (setq ninja--github-token-cache nil)
     nil)))

(provide 'ninja-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
