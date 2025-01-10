;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:12:33
;;; Time-stamp: <2025-01-06 20:12:33 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/04-git.el



;; Git command wrapper
(defun llemacs--run-git-command (bin &rest args)
  (llemacs--logging-write-info-pj
   (format "Running git command: %s" args))
  (let ((cmd (concat bin " " (string-join args " "))))
    (llemacs--run-shell-command cmd)))

;; Ensure branch exists
(defun llemacs--git-ensure-branch (bin branch)
  (condition-case nil
      (llemacs--run-git-command bin "checkout" branch)
    (llemacs--logging-write-error-pj
     (llemacs--run-git-command bin "checkout" "-b" branch))))

;; Core Git operations
(defun llemacs--git-add (bin files)
  (dolist (file files)
    (llemacs--run-git-command bin "add" file)))

(defun llemacs--git-commit (bin message)
  (llemacs--run-git-command bin "commit" "-m" message))

(defun llemacs--git-push (bin branch)
  (llemacs--run-git-command bin "push" "origin" branch))

(defun llemacs--commit-and-push (files message branch)
  (condition-case err
      (let ((git-bin (llemacs--path-find-bin "git")))
        (llemacs--git-ensure-branch git-bin branch)
        (llemacs--git-add git-bin files)
        (llemacs--git-commit git-bin message)
        (llemacs--git-push git-bin branch))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj
      (format "Git operations failed: %s" err)))))
