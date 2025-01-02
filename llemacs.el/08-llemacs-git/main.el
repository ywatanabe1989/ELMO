;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 12:07:55
;;; Time-stamp: <2025-01-02 12:07:55 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/08-llemacs-git/main.el

(defgroup llemacs-git nil
  "Git operations for Llemacs."
  :group 'llemacs)

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

(defcustom llemacs-git-prompt-for-message t
  "Whether to prompt for commit messages."
  :type 'boolean
  :group 'llemacs-git)


;; git-configure
;; git-ensure
;; git-init
;; git-status
;; git-switch
;; git-branch
;; git-add
;; git-commit
;; git-genai-commit
;; git-push
;; git-branch
;; git-setup-gitignore
;; git-reset
;; git-unstage
;; git-track
;; git-track
;; git-encrypt-secrets
;; git-merge
;; git-pull
;; git-tree

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
         (branch (or branch "main")))
    (call-process "git" nil nil nil "push" remote branch)))

(defun llemacs--git-setup-gitignore (dir)
  "Setup .gitignore in DIR with common patterns."
  (let ((gitignore (expand-file-name ".gitignore" dir)))
    (unless (file-exists-p gitignore)
      (with-temp-file gitignore
        (insert "*.elc\n"
                "*~\n"
                "*.log\n"
                ".DS_Store\n"
                "auto-save-list\n"
                "elpa/\n"
                ".git/\n"
                "backups/\n")))))

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