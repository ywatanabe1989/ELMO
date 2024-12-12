;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:44:15
;;; Time-stamp: <2024-12-02 06:44:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-verify-installation.el


(require 'ninja-config)
(require 'ert)

(ert-deftest test-ninja-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (ninja-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (ninja-check-dependencies))))

(ert-deftest test-ninja-create-directories ()
  "Test directory creation."
  (let ((ninja-work-dir (make-temp-file "ninja-test-" t))
        (ninja-workspace-dir (make-temp-file "ninja-workspace-" t))
        (ninja-source-dir (make-temp-file "ninja-source-" t)))
    (unwind-protect
        (progn
          (ninja-create-directories)
          (should (file-directory-p ninja-work-dir))
          (should (file-directory-p ninja-workspace-dir))
          (should (file-directory-p ninja-source-dir)))
      (delete-directory ninja-work-dir t)
      (delete-directory ninja-workspace-dir t)
      (delete-directory ninja-source-dir t))))

(ert-deftest test-ninja-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "ninja-test-" t))
         (ninja-user-request-file (expand-file-name "user-request.md" temp-dir))
         (ninja-request-file (expand-file-name "request.md" temp-dir))
         (ninja-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (ninja-create-initial-files)
          (should (file-exists-p ninja-user-request-file))
          (should (file-exists-p ninja-request-file))
          (should (file-exists-p ninja-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-ninja-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "ninja-test-" t))
         (ninja-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (ninja-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "NINJA_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-ninja-verify-installation ()
  "Test installation verification."
  (let* ((temp-dir (make-temp-file "ninja-test-" t))
         (ninja-work-dir temp-dir)
         (ninja-workspace-dir (expand-file-name "workspace" temp-dir))
         (ninja-source-dir (expand-file-name "source" temp-dir))
         (ninja-logs-dir (expand-file-name "logs" temp-dir))
         (ninja-config-dir (expand-file-name "config" temp-dir))
         (ninja-github-token-file (expand-file-name "token" temp-dir))
         (ninja-user-request-file (expand-file-name "user-request.md" temp-dir))
         (ninja-request-file (expand-file-name "request.md" temp-dir))
         (ninja-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list ninja-workspace-dir ninja-source-dir ninja-logs-dir ninja-config-dir))
          (mapc (lambda (file) (with-temp-file file (insert "test")))
                (list ninja-github-token-file ninja-user-request-file ninja-request-file ninja-history-file))
          (should (progn (ninja-verify-installation) t)))
      (delete-directory temp-dir t))))

(ert-deftest test-ninja-setup-github-token ()
  "Test GitHub token setup."
  (let* ((temp-dir (make-temp-file "ninja-test-" t))
         (ninja-github-token-file (expand-file-name "github-token" temp-dir))
         (test-token "ghp_test1234567890"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) test-token)))
            (ninja-setup-github-token)
            (should (file-exists-p ninja-github-token-file))
            (should (string= (with-temp-buffer
                             (insert-file-contents ninja-github-token-file)
                             (string-trim (buffer-string)))
                           test-token))))
      (delete-directory temp-dir t))))

(ert-deftest test-ninja-full-installation-process ()
  "Integration test for the full installation process."
  (let* ((temp-root (make-temp-file "ninja-test-root-" t))
         (ninja-work-dir (expand-file-name "ninja" temp-root))
         (ninja-workspace-dir (expand-file-name "workspace" ninja-work-dir))
         (ninja-source-dir (expand-file-name "source" ninja-work-dir))
         (ninja-config-dir (expand-file-name "config" ninja-work-dir))
         (test-token "ghp_testtoken12345"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) test-token))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (ninja-install)
          (should (file-directory-p ninja-work-dir))
          (should (file-directory-p ninja-workspace-dir))
          (should (file-directory-p ninja-source-dir))
          (should (file-directory-p ninja-config-dir))
          (should (file-exists-p (expand-file-name ".env" ninja-config-dir)))
          (should (file-exists-p (expand-file-name "github-token" ninja-config-dir)))
          (should (ninja-verify-installation)))
      (delete-directory temp-root t))))

;; Main verification function
(defun ninja-verify-installation ()
  "Verify that all NINJA components are properly installed."
  (and (file-exists-p ninja-work-dir)
       (file-exists-p ninja-workspace-dir)
       (file-exists-p ninja-source-dir)
       (file-exists-p ninja-logs-dir)
       (file-exists-p ninja-config-dir)
       (file-exists-p ninja-github-token-file)))

(provide 'ninja-verify-installation)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
