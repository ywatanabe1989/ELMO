;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:44:15
;;; Time-stamp: <2024-12-02 06:44:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-verify-installation.el


(require 'sea-config)
(require 'ert)

(ert-deftest test-sea-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (sea-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (sea-check-dependencies))))

(ert-deftest test-sea-create-directories ()
  "Test directory creation."
  (let ((sea-work-dir (make-temp-file "sea-test-" t))
        (sea-workspace-dir (make-temp-file "sea-workspace-" t))
        (sea-source-dir (make-temp-file "sea-source-" t)))
    (unwind-protect
        (progn
          (sea-create-directories)
          (should (file-directory-p sea-work-dir))
          (should (file-directory-p sea-workspace-dir))
          (should (file-directory-p sea-source-dir)))
      (delete-directory sea-work-dir t)
      (delete-directory sea-workspace-dir t)
      (delete-directory sea-source-dir t))))

(ert-deftest test-sea-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-user-request-file (expand-file-name "user-request.md" temp-dir))
         (sea-request-file (expand-file-name "request.md" temp-dir))
         (sea-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (sea-create-initial-files)
          (should (file-exists-p sea-user-request-file))
          (should (file-exists-p sea-request-file))
          (should (file-exists-p sea-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-sea-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (sea-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "SEA_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-sea-verify-installation ()
  "Test installation verification."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-work-dir temp-dir)
         (sea-workspace-dir (expand-file-name "workspace" temp-dir))
         (sea-source-dir (expand-file-name "source" temp-dir))
         (sea-logs-dir (expand-file-name "logs" temp-dir))
         (sea-config-dir (expand-file-name "config" temp-dir))
         (sea-github-token-file (expand-file-name "token" temp-dir))
         (sea-user-request-file (expand-file-name "user-request.md" temp-dir))
         (sea-request-file (expand-file-name "request.md" temp-dir))
         (sea-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list sea-workspace-dir sea-source-dir sea-logs-dir sea-config-dir))
          (mapc (lambda (file) (with-temp-file file (insert "test")))
                (list sea-github-token-file sea-user-request-file sea-request-file sea-history-file))
          (should (progn (sea-verify-installation) t)))
      (delete-directory temp-dir t))))

(ert-deftest test-sea-setup-github-token ()
  "Test GitHub token setup."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-github-token-file (expand-file-name "github-token" temp-dir))
         (test-token "ghp_test1234567890"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) test-token)))
            (sea-setup-github-token)
            (should (file-exists-p sea-github-token-file))
            (should (string= (with-temp-buffer
                             (insert-file-contents sea-github-token-file)
                             (string-trim (buffer-string)))
                           test-token))))
      (delete-directory temp-dir t))))

(ert-deftest test-sea-full-installation-process ()
  "Integration test for the full installation process."
  (let* ((temp-root (make-temp-file "sea-test-root-" t))
         (sea-work-dir (expand-file-name "sea" temp-root))
         (sea-workspace-dir (expand-file-name "workspace" sea-work-dir))
         (sea-source-dir (expand-file-name "source" sea-work-dir))
         (sea-config-dir (expand-file-name "config" sea-work-dir))
         (test-token "ghp_testtoken12345"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) test-token))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (sea-install)
          (should (file-directory-p sea-work-dir))
          (should (file-directory-p sea-workspace-dir))
          (should (file-directory-p sea-source-dir))
          (should (file-directory-p sea-config-dir))
          (should (file-exists-p (expand-file-name ".env" sea-config-dir)))
          (should (file-exists-p (expand-file-name "github-token" sea-config-dir)))
          (should (sea-verify-installation)))
      (delete-directory temp-root t))))

;; Main verification function
(defun sea-verify-installation ()
  "Verify that all SEA components are properly installed."
  (and (file-exists-p sea-work-dir)
       (file-exists-p sea-workspace-dir)
       (file-exists-p sea-source-dir)
       (file-exists-p sea-logs-dir)
       (file-exists-p sea-config-dir)
       (file-exists-p sea-github-token-file)))

(provide 'sea-verify-installation)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
