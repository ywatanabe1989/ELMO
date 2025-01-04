;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 15:46:01
;;; Time-stamp: <2025-01-04 15:46:01 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/08-llemacs-git/test/test-llemacs-git.el


;;; test-llemacs-git.el --- Git functionality tests -*- lexical-binding: t -*-

(require 'ert)
(require 'llemacs)

;; Test helpers
(defmacro with-test-git-repo (&rest body)
  "Create temporary git repo and execute BODY within it."
  `(let* ((temp-dir (make-temp-file "test-git-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           (llemacs--git-init temp-dir)
           ,@body)
       (delete-directory temp-dir t))))

;; Test cases
(ert-deftest test-llemacs-git-init ()
  "Test git repository initialization."
  (with-test-git-repo
   (should (file-exists-p ".git"))
   (should (file-directory-p ".git"))))

(ert-deftest test-llemacs-git-add-commit ()
  "Test git add and commit operations."
  (with-test-git-repo
   ;; Create test file
   (write-region "test" nil "test.txt")
   ;; Test add and commit
   (llemacs--git-add-and-commit default-directory "Test commit")
   ;; Verify commit
   (should (string-match-p "Test commit"
                           (car (llemacs--git-log default-directory 1))))))

(ert-deftest test-llemacs-git-ignore ()
  "Test gitignore setup."
  (with-test-git-repo
   (llemacs--git-setup-gitignore default-directory)
   (should (file-exists-p ".gitignore"))
   (should (string-match-p "*.elc"
                           (with-temp-buffer
                             (insert-file-contents ".gitignore")
                             (buffer-string))))))

(ert-deftest test-llemacs-git-track-untrack ()
  "Test file tracking operations."
  (with-test-git-repo
   ;; Create test files
   (write-region "test1" nil "test1.txt")
   (write-region "test2" nil "test2.txt")
   ;; Test tracking
   (llemacs--git-track default-directory '("test1.txt"))
   (should (string-match-p "test1.txt"
                           (shell-command-to-string "git ls-files")))
   ;; Test untracking
   (llemacs--git-untrack default-directory '("test2.txt"))
   (should-not (string-match-p "test2.txt"
                               (shell-command-to-string "git ls-files")))))

(ert-deftest test-llemacs-git-reset ()
  "Test git reset functionality."
  (with-test-git-repo
   ;; Create and commit first change
   (write-region "test1" nil "test.txt")
   (llemacs--git-add-and-commit default-directory "First commit")
   ;; Create and commit second change
   (write-region "test2" nil "test.txt")
   (llemacs--git-add-and-commit default-directory "Second commit")
   ;; Test reset
   (llemacs--git-reset default-directory 1)
   (should (string-match-p "First commit"
                           (car (llemacs--git-log default-directory 1))))))

(ert-deftest test-llemacs-git-unstage ()
  "Test git unstage functionality."
  (with-test-git-repo
   ;; Create and stage file
   (write-region "test" nil "test.txt")
   (llemacs--git-add default-directory)
   ;; Test unstage
   (llemacs--git-unstage default-directory)
   (should-not (string-match-p "test.txt"
                               (shell-command-to-string "git diff --cached --name-only")))))

(provide 'test-llemacs-git)
;;; test-llemacs-git.el ends here


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))