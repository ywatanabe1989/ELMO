;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 08:58:01
;;; Time-stamp: <2024-12-04 08:58:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'elmo-utils)
(require 'elmo-prompts)

(defgroup elmo-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'elmo)

(defcustom elmo-git-user-name "elmo"
  "Git user name for ELMO commits."
  :type 'string
  :group 'elmo-git)

(defcustom elmo-git-user-email "elmo@example.com"
  "Git email for ELMO commits."
  :type 'string
  :group 'elmo-git)

(defcustom elmo-github-token-file "~/.config/elmo/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'elmo-git)

(defvar elmo-github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

;; (defun elmo-validate-github-token (token)
;;   "Validate TOKEN format and basic structure."
;;   (when (or (null token)
;;             (not (stringp token))
;;             (string-empty-p token)
;;             (< (length token) 40))
;;     (error "Invalid GitHub token format")))

(defun elmo-validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (elmo-log-message "Invalid GitHub token format")
    (error "Invalid GitHub token format")))

;; (defun elmo-load-github-token ()
;;   "Load GitHub token from file with validation and error handling."
;;   (condition-case err
;;       (let* ((token-file (expand-file-name elmo-github-token-file))
;;              (real-file (file-truename token-file)))
;;         (unless (file-exists-p real-file)
;;           (error "GitHub token file not found: %s" real-file))
;;         (unless (file-readable-p real-file)
;;           (error "GitHub token file not readable: %s" real-file))
;;         (let ((token (with-temp-buffer
;;                       (insert-file-contents real-file)
;;                       (string-trim (buffer-string)))))
;;           (elmo-validate-github-token token)
;;           token))
;;     (error
;;      (message "Failed to load GitHub token: %s" (error-message-string err))
;;      nil)))

(defun elmo-load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name elmo-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (elmo-log-message (format "GitHub token file not found: %s" real-file))
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (elmo-log-message (format "GitHub token file not readable: %s" real-file))
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (elmo-validate-github-token token)
          token))
    (error
     (elmo-log-message (format "Failed to load GitHub token: %s" err))
     nil)))

(defun elmo-get-github-token ()
  "Get GitHub token, using cache if available."
  (or elmo-github-token-cache
      (setq elmo-github-token-cache (elmo-load-github-token))))

;; Replace existing initialization with:
(elmo-get-github-token)

;; (defun elmo-ensure-not-main ()
;;   "Ensure we're not on main branch."
;;   (let ((current-branch
;;          (string-trim
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
;;     (when (string= current-branch "main")
;;       (error "Cannot modify main branch directly"))))
(defun elmo-ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (elmo-log-message "Cannot modify main branch directly")
      (error "Cannot modify main branch directly"))))

;; (defun elmo-commit-changes (file msg)
;;   "Commit changes to FILE with commit MSG."
;;   (elmo-ensure-not-main)
;;   (let ((default-directory (file-name-directory file)))
;;     (elmo-shell-command
;;      (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
;;              elmo-git-user-name
;;              elmo-git-user-email
;;              (file-name-nondirectory file)
;;              elmo-git-user-name
;;              elmo-git-user-email
;;              msg))))
(defun elmo-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (condition-case err
      (progn
        (elmo-ensure-not-main)
        (let ((default-directory (file-name-directory file)))
          (elmo-shell-command
           (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
                   elmo-git-user-name elmo-git-user-email
                   (file-name-nondirectory file)
                   elmo-git-user-name elmo-git-user-email msg))
          (elmo-log-message (format "Committed changes to %s" file))))
    (error
     (elmo-log-message (format "Failed to commit changes: %s" err))
     nil)))

;; (defun elmo-push-changes ()
;;   "Push changes to elmo-develop branch."
;;   (elmo-ensure-not-main)
;;   (elmo-shell-command
;;    (concat
;;     "git checkout -b elmo-develop 2>/dev/null || git checkout elmo-develop && "
;;     "git push -u origin elmo-develop")))
(defun elmo-push-changes ()
  "Push changes to elmo-develop branch."
  (condition-case err
      (progn
        (elmo-ensure-not-main)
        (elmo-shell-command
         (concat "git checkout -b elmo-develop 2>/dev/null || git checkout elmo-develop && "
                "git push -u origin elmo-develop"))
        (elmo-log-message "Pushed changes to elmo-develop branch"))
    (error
     (elmo-log-message (format "Failed to push changes: %s" err))
     nil)))

;; (defun elmo-create-pr (title body)
;;   "Create pull request with TITLE and BODY from elmo-develop to main."
;;   (let ((token (elmo-get-github-token)))
;;     (unless token
;;       (error "GitHub token not available"))
;;     (let ((url "https://api.github.com/repos/owner/repo/pulls")
;;           (headers `(("Authorization" . ,(concat "token " token))
;;                     ("Accept" . "application/vnd.github.v3+json")))
;;           (data (json-encode
;;                  `((title . ,title)
;;                    (body . ,body)
;;                    (head . "elmo-develop")
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
;;          (setq elmo-github-token-cache nil)
;;          (error "Failed to create PR: %s" (error-message-string err)))))))

(defun elmo-create-pr (title body)
  "Create pull request with TITLE and BODY from elmo-develop to main."
  (condition-case err
      (let ((token (elmo-get-github-token)))
        (unless token
          (elmo-log-message "GitHub token not available")
          (error "GitHub token not available"))
        (request "https://api.github.com/repos/owner/repo/pulls"
                 :type "POST"
                 :headers `(("Authorization" . ,(concat "token " token))
                          ("Accept" . "application/vnd.github.v3+json"))
                 :data (json-encode
                       `((title . ,title)
                         (body . ,body)
                         (head . "elmo-develop")
                         (base . "main")))
                 :parser 'json-read
                 :success (lambda (&rest _)
                           (elmo-log-message "Pull request created successfully"))
                 :error (lambda (&rest args)
                         (elmo-log-message (format "PR creation failed: %S" args))
                         (error "PR creation failed: %S" args))))
    (error
     (elmo-log-message (format "Failed to create PR: %s" err))
     (setq elmo-github-token-cache nil)
     nil)))

(provide 'elmo-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
