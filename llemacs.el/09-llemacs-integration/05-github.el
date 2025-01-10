;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:10:44
;;; Time-stamp: <2025-01-06 20:10:44 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/05-github.el

(defun llemacs--ensure-gh-auth ()
  (condition-case err
      (llemacs--run-gh-command "auth" "status")
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-info-pj "Authenticating with GitHub...")
     (llemacs--run-gh-command "auth" "login"))))

(defun llemacs--commit-and-push (files message branch)
  (condition-case err
      (let ((git-bin (llemacs--path-find-bin "git")))
        (dolist (file files)
          (llemacs--run-git-command git-bin "add" file))
        (llemacs--run-git-command git-bin "commit" "-m" message)
        (llemacs--run-git-command git-bin "push" "origin" branch))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj
      (format "Git operations failed: %s" err)))))

(defun llemacs--create-ticket (title body)
  (condition-case err
      (let ((gh-bin (llemacs--path-find-bin "gh")))
        (llemacs--ensure-gh-auth)
        (llemacs--run-gh-command gh-bin "issue" "create"
                                 "--title" title
                                 "--body" body))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj
      (format "Failed to create ticket: %s" err)))))

;; GitHub CLI wrapper
(defun llemacs--run-gh-command (bin &rest args)
  (llemacs--logging-write-info-pj
   (format "Running GitHub CLI command: %s" args))
  (let ((cmd (concat bin " " (string-join args " "))))
    (llemacs--run-shell-command cmd)))

;; Authentication
(defun llemacs--ensure-gh-auth ()
  (condition-case err
      (let ((gh-bin (llemacs--path-find-bin "gh")))
        (llemacs--run-gh-command gh-bin "auth" "status"))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-info-pj "Authenticating with GitHub...")
     (llemacs--run-gh-command gh-bin "auth" "login"))))

;; Core GitHub operations
(defun llemacs--gh-create-issue (bin title body)
  (llemacs--run-gh-command bin "issue" "create"
                           "--title" title
                           "--body" body))

(defun llemacs--gh-create-pr (bin title body base)
  (llemacs--run-gh-command bin "pr" "create"
                           "--title" title
                           "--body" body
                           "--base" base))

(defun llemacs--create-ticket (title body)
  (condition-case err
      (let ((gh-bin (llemacs--path-find-bin "gh")))
        (llemacs--ensure-gh-auth)
        (llemacs--gh-create-issue gh-bin title body))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj
      (format "Failed to create ticket: %s" err)))))
