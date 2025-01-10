;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 05:34:14
;;; Timestamp: <2025-01-08 05:34:14>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/07-llemacs-tools/01-git.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--git-enabled t
  "Whether to enable Git integration."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs--git-auto-commit t
  "Whether to automatically commit changes."
  :type 'boolean
  :group 'llemacs)

(defcustom llemacs-git-auto-add t
  "Whether to automatically stage modified files."
  :type 'boolean
  :group 'llemacs-git)

(defvar llemacs--git-gitignore-path
  (getenv "LLEMACS_GIT_GITIGNORE_PATH")
  "Path to global gitignore template file.")

(defun llemacs--git-with-recipe-config (dir recipe-name func &rest args)
  "Execute FUNC with recipe-specific git config in DIR."
  (let* ((default-directory dir)
         (git-bin (llemacs--path-find-bin "git"))
         (agent-email (getenv "LLEMACS_AGENT_EMAIL_ADDRESS"))
         (agent-password (getenv "LLEMACS_AGENT_EMAIL_PASSWORD")))
    (llemacs--logging-write-debug-pj
     (format "Setting up git config for recipe: %s" recipe-name))
    (unless agent-email
      (llemacs--logging-write-error-pj "LLEMACS_AGENT_EMAIL_ADDRESS is not set"))
    (unless agent-password
      (llemacs--logging-write-error-pj "LLEMACS_AGENT_EMAIL_PASSWORD is not set"))
    (let* ((process-environment
            (cons* "GIT_AUTHOR_EMAIL" agent-email
                   "GIT_AUTHOR_PASSWORD" agent-password
                   process-environment))
           (git-args (list "--config-env=user.email=GIT_AUTHOR_EMAIL"
                           "--config-env=user.password=GIT_AUTHOR_PASSWORD")))
      (condition-case err
          (progn
            (llemacs--logging-write-info-pj
             (format "Executing git command in directory: %s" dir))
            (apply func (append git-args args)))
        (llemacs--logging-write-error-pj
         (llemacs--logging-write-error-pj
          (format "Git operation failed: %s" (error-message-string err)))
         nil)))))

(defun llemacs--git-configure ()
  "Configure git with user credentials from environment."
  (let ((git-bin (llemacs--path-find-bin "git"))
        (email (getenv "LLEMACS_GIT_EMAIL"))
        (name (getenv "LLEMACS_GIT_USER_NAME"))
        (template-dir (getenv "LLEMACS_GIT_TEMPLATE_DIR")))
    (llemacs--logging-write-debug-pj
     (format "Git configuration: bin=%s, email=%s, name=%s, template-dir=%s"
             git-bin email name template-dir))
    (when email
      (llemacs--logging-write-debug-pj
       (format "Setting git email to: %s" email))
      (llemacs--run-git-command git-bin "config" "--global" "user.email" email))
    (when name
      (llemacs--logging-write-debug-pj
       (format "Setting git user name to: %s" name))
      (llemacs--run-git-command git-bin "config" "--global" "user.name" name))
    (when template-dir
      (llemacs--logging-write-debug-pj
       (format "Setting git template directory to: %s" template-dir))
      (llemacs--run-git-command git-bin "config" "--global" "init.templateDir" template-dir))))


(defun llemacs--git-init (dir)
  "Initialize Git repository in DIR if not already initialized."
  (unless (file-exists-p (expand-file-name ".git" dir))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init"))))

(defun llemacs--git-add-and-commit (dir message)
  "Stage all changes in DIR and commit with MESSAGE."
  (when (and llemacs--git-enabled llemacs--git-auto-commit)
    (let ((default-directory dir))
      (call-process "git" nil nil nil "add" ".")
      (call-process "git" nil nil nil "commit" "-m" message))))

(defun llemacs--git-log (dir &optional limit)
  "Get git log for DIR with optional LIMIT."
  (let* ((default-directory dir)
         (limit-arg (if limit (format "-%d" limit) "-10"))
         (output (shell-command-to-string
                  (format "git log %s --oneline" limit-arg))))
    (split-string output "\n" t)))

(defun llemacs--git-ensure-repo (dir)
  "Initialize git repository in DIR if not exists."
  (unless (file-exists-p (expand-file-name ".git" dir))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init"))))

(defun llemacs--git-add (dir &optional files)
  "Stage FILES (or all if nil) in DIR."
  (let ((default-directory dir))
    (if files
        (dolist (file files)
          (call-process "git" nil nil nil "add" file))
      (call-process "git" nil nil nil "add" "."))))

(defun llemacs--git-commit (dir message)
  "Commit changes in DIR with MESSAGE."
  (let ((default-directory dir))
    (call-process "git" nil nil nil "commit" "-m" message)))

(defun llemacs--git-push (dir &optional remote branch)
  "Push changes in DIR to REMOTE BRANCH."
  (let* ((default-directory dir)
         (remote (or remote "origin"))
         (branch (or branch "llemacs")))
    (call-process "git" nil nil nil "push" remote branch)))


(defun llemacs--git-setup-gitignore (dir)
  "Setup .gitignore in DIR with common patterns."
  (let ((gitignore (expand-file-name ".gitignore" dir)))
    (unless (file-exists-p gitignore)
      (if llemacs--git-gitignore-path
          ;; Copy from template if exists
          (copy-file llemacs--git-gitignore-path gitignore)
        ;; Otherwise use default patterns
        (with-temp-file gitignore
          (insert "*.elc\n"
                  "*~\n"
                  "*.log\n"
                  ".DS_Store\n"
                  "auto-save-list\n"
                  "elpa/\n"
                  ".git/\n"
                  "backups/\n"))))))

(defun llemacs--git-reset (dir &optional n-commits)
  "Reset last N-COMMITS in DIR."
  (let ((default-directory dir)
        (n (or n-commits 1)))
    (call-process "git" nil nil nil "reset" (format "HEAD~%d" n))))

(defun llemacs--git-unstage (dir &optional files)
  "Unstage FILES (or all) in DIR."
  (let ((default-directory dir))
    (if files
        (dolist (file files)
          (call-process "git" nil nil nil "restore" "--staged" file))
      (call-process "git" nil nil nil "restore" "--staged" "."))))

(defun llemacs--git-track (dir files)
  "Track FILES in DIR repository."
  (let ((default-directory dir))
    (dolist (file files)
      (call-process "git" nil nil nil "add" file)
      (with-temp-buffer
        (insert-file-contents ".gitignore")
        (goto-char (point-min))
        (while (re-search-forward (format "^%s$" (regexp-quote file)) nil t)
          (delete-region (line-beginning-position) (1+ (line-end-position))))
        (write-region (point-min) (point-max) ".gitignore")))))

(defun llemacs--git-untrack (dir files)
  "Untrack FILES in DIR repository."
  (let ((default-directory dir))
    (dolist (file files)
      (call-process "git" nil nil nil "rm" "--cached" "-r" file)
      (with-temp-file ".gitignore"
        (insert file "\n")))))

;; gh-dowlonad-dir
;; gh-login
;; gh-login-with-token
;; gh-logout
;; gh-view
;; gh-add-remote
;; gh-create
;; gh-rename
;; gh-clone
;; gh-protect-main

(defun llemacs--git-protect-main (dir)
  "Protect main branch in DIR repository."
  (let ((default-directory dir))
    (call-process "gh" nil nil nil "api"
                  "--method" "PUT"
                  "-H" "Accept: application/vnd.github+json"
                  "/repos/{owner}/{repo}/branches/main/protection"
                  "-f" "required_status_checks='{\"strict\":true,\"contexts\":[]}'"
                  "-f" "enforce_admins=true"
                  "-f" "required_pull_request_reviews='{\"dismissal_restrictions\":{},\"dismiss_stale_reviews\":true,\"require_code_owner_reviews\":true,\"required_approving_review_count\":1}'"
                  "-f" "restrictions=null")))

(defun llemacs--git-resolve-conflicts (dir)
  "Resolve merge conflicts in DIR."
  (let ((default-directory dir))
    (dolist (file (split-string (shell-command-to-string "git diff --name-only --diff-filter=U") "\n" t))
      (when (file-exists-p file)
        (with-temp-file file
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^<<<<<<< HEAD$" nil t)
            (let ((start (point))
                  (middle (progn (re-search-forward "^=======$") (match-beginning 0)))
                  (end (progn (re-search-forward "^>>>>>>> .*$") (match-end 0))))
              (delete-region (line-beginning-position 0) end)
              (goto-char start))))))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
