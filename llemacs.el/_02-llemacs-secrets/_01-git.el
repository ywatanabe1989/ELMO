;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-07 07:54:58
;;; Time-stamp: <2025-01-07 07:54:58 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-lemacs-secrets/01-git.el

;; Git configuration function
(defun llemacs--secret-git-configure ()
  "Configure git with user credentials from environment."
  (let ((git-bin (llemacs--path-find-bin "git"))
        (email (getenv "LLEMACS_GIT_EMAIL"))
        (name (getenv "LLEMACS_GIT_USER_NAME"))
        (template-dir (getenv "LLEMACS_GIT_TEMPLATE_DIR")))

    (when email
      (llemacs--run-git-command git-bin "config" "--global" "user.email" email))

    (when name
      (llemacs--run-git-command git-bin "config" "--global" "user.name" name))

    (when template-dir
      (llemacs--run-git-command git-bin "config" "--global" "init.templateDir" template-dir))))

;; GitHub login function
(defun llemacs--secret-github-login ()
  "Login to GitHub using token from environment."
  (let ((token-path (getenv "LLEMACS_GITHUB_TOKEN_PATH")))
    (when token-path
      (let ((token (with-temp-buffer
                     (insert-file-contents token-path)
                     (string-trim (buffer-string)))))
        (llemacs--store-access-token "github" token)
        (llemacs--ensure-gh-auth)))))
