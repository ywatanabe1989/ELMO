;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:44:15
;;; Time-stamp: <2024-12-02 06:44:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-verify-installation.el


(require 'semacs-config)
(require 'ert)

(ert-deftest test-semacs-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (semacs-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (semacs-check-dependencies))))

(ert-deftest test-semacs-create-directories ()
  "Test directory creation."
  (let ((semacs-work-dir (make-temp-file "semacs-test-" t))
        (semacs-workspace-dir (make-temp-file "semacs-workspace-" t))
        (semacs-source-dir (make-temp-file "semacs-source-" t)))
    (unwind-protect
        (progn
          (semacs-create-directories)
          (should (file-directory-p semacs-work-dir))
          (should (file-directory-p semacs-workspace-dir))
          (should (file-directory-p semacs-source-dir)))
      (delete-directory semacs-work-dir t)
      (delete-directory semacs-workspace-dir t)
      (delete-directory semacs-source-dir t))))

(ert-deftest test-semacs-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "semacs-test-" t))
         (semacs-user-request-file (expand-file-name "user-request.md" temp-dir))
         (semacs-request-file (expand-file-name "request.md" temp-dir))
         (semacs-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (semacs-create-initial-files)
          (should (file-exists-p semacs-user-request-file))
          (should (file-exists-p semacs-request-file))
          (should (file-exists-p semacs-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-semacs-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "semacs-test-" t))
         (semacs-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (semacs-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "SEMACS_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-semacs-verify-installation ()
  "Test installation verification."
  (let* ((temp-dir (make-temp-file "semacs-test-" t))
         (semacs-work-dir temp-dir)
         (semacs-workspace-dir (expand-file-name "workspace" temp-dir))
         (semacs-source-dir (expand-file-name "source" temp-dir))
         (semacs-logs-dir (expand-file-name "logs" temp-dir))
         (semacs-config-dir (expand-file-name "config" temp-dir))
         (semacs-github-token-file (expand-file-name "token" temp-dir))
         (semacs-user-request-file (expand-file-name "user-request.md" temp-dir))
         (semacs-request-file (expand-file-name "request.md" temp-dir))
         (semacs-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list semacs-workspace-dir semacs-source-dir semacs-logs-dir semacs-config-dir))
          (mapc (lambda (file) (with-temp-file file (insert "test")))
                (list semacs-github-token-file semacs-user-request-file semacs-request-file semacs-history-file))
          (should (progn (semacs-verify-installation) t)))
      (delete-directory temp-dir t))))

(ert-deftest test-semacs-setup-github-token ()
  "Test GitHub token setup."
  (let* ((temp-dir (make-temp-file "semacs-test-" t))
         (semacs-github-token-file (expand-file-name "github-token" temp-dir))
         (test-token "ghp_test1234567890"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) test-token)))
            (semacs-setup-github-token)
            (should (file-exists-p semacs-github-token-file))
            (should (string= (with-temp-buffer
                             (insert-file-contents semacs-github-token-file)
                             (string-trim (buffer-string)))
                           test-token))))
      (delete-directory temp-dir t))))

(ert-deftest test-semacs-full-installation-process ()
  "Integration test for the full installation process."
  (let* ((temp-root (make-temp-file "semacs-test-root-" t))
         (semacs-work-dir (expand-file-name "semacs" temp-root))
         (semacs-workspace-dir (expand-file-name "workspace" semacs-work-dir))
         (semacs-source-dir (expand-file-name "source" semacs-work-dir))
         (semacs-config-dir (expand-file-name "config" semacs-work-dir))
         (test-token "ghp_testtoken12345"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) test-token))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (semacs-install)
          (should (file-directory-p semacs-work-dir))
          (should (file-directory-p semacs-workspace-dir))
          (should (file-directory-p semacs-source-dir))
          (should (file-directory-p semacs-config-dir))
          (should (file-exists-p (expand-file-name ".env" semacs-config-dir)))
          (should (file-exists-p (expand-file-name "github-token" semacs-config-dir)))
          (should (semacs-verify-installation)))
      (delete-directory temp-root t))))

;; Main verification function
(defun semacs-verify-installation ()
  "Verify that all SEMACS components are properly installed."
  (and (file-exists-p semacs-work-dir)
       (file-exists-p semacs-workspace-dir)
       (file-exists-p semacs-source-dir)
       (file-exists-p semacs-logs-dir)
       (file-exists-p semacs-config-dir)
       (file-exists-p semacs-github-token-file)))

(provide 'semacs-verify-installation)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
