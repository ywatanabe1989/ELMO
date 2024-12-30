;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:44:15
;;; Time-stamp: <2024-12-02 06:44:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-verify-installation.el


(require 'llemacs-config)
(require 'ert)

(ert-deftest test-llemacs-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (llemacs-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (llemacs-check-dependencies))))

(ert-deftest test-llemacs-create-directories ()
  "Test directory creation."
  (let ((llemacs-work-dir (make-temp-file "llemacs-test-" t))
        (llemacs-workspace-dir (make-temp-file "llemacs-workspace-" t))
        (llemacs-source-dir (make-temp-file "llemacs-source-" t)))
    (unwind-protect
        (progn
          (llemacs-create-directories)
          (should (file-directory-p llemacs-work-dir))
          (should (file-directory-p llemacs-workspace-dir))
          (should (file-directory-p llemacs-source-dir)))
      (delete-directory llemacs-work-dir t)
      (delete-directory llemacs-workspace-dir t)
      (delete-directory llemacs-source-dir t))))

(ert-deftest test-llemacs-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "llemacs-test-" t))
         (llemacs-user-request-file (expand-file-name "user-request.md" temp-dir))
         (llemacs-request-file (expand-file-name "request.md" temp-dir))
         (llemacs-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (llemacs-create-initial-files)
          (should (file-exists-p llemacs-user-request-file))
          (should (file-exists-p llemacs-request-file))
          (should (file-exists-p llemacs-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "llemacs-test-" t))
         (llemacs-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (llemacs-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "ELMO_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-verify-installation ()
  "Test installation verification."
  (let* ((temp-dir (make-temp-file "llemacs-test-" t))
         (llemacs-work-dir temp-dir)
         (llemacs-workspace-dir (expand-file-name "workspace" temp-dir))
         (llemacs-source-dir (expand-file-name "source" temp-dir))
         (llemacs-logs-dir (expand-file-name "logs" temp-dir))
         (llemacs-config-dir (expand-file-name "config" temp-dir))
         (llemacs-github-token-file (expand-file-name "token" temp-dir))
         (llemacs-user-request-file (expand-file-name "user-request.md" temp-dir))
         (llemacs-request-file (expand-file-name "request.md" temp-dir))
         (llemacs-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list llemacs-workspace-dir llemacs-source-dir llemacs-logs-dir llemacs-config-dir))
          (mapc (lambda (file) (with-temp-file file (insert "test")))
                (list llemacs-github-token-file llemacs-user-request-file llemacs-request-file llemacs-history-file))
          (should (progn (llemacs-verify-installation) t)))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-setup-github-token ()
  "Test GitHub token setup."
  (let* ((temp-dir (make-temp-file "llemacs-test-" t))
         (llemacs-github-token-file (expand-file-name "github-token" temp-dir))
         (test-token "ghp_test1234567890"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) test-token)))
            (llemacs-setup-github-token)
            (should (file-exists-p llemacs-github-token-file))
            (should (string= (with-temp-buffer
                             (insert-file-contents llemacs-github-token-file)
                             (string-trim (buffer-string)))
                           test-token))))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-full-installation-process ()
  "Integration test for the full installation process."
  (let* ((temp-root (make-temp-file "llemacs-test-root-" t))
         (llemacs-work-dir (expand-file-name "llemacs" temp-root))
         (llemacs-workspace-dir (expand-file-name "workspace" llemacs-work-dir))
         (llemacs-source-dir (expand-file-name "source" llemacs-work-dir))
         (llemacs-config-dir (expand-file-name "config" llemacs-work-dir))
         (test-token "ghp_testtoken12345"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) test-token))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (llemacs-install)
          (should (file-directory-p llemacs-work-dir))
          (should (file-directory-p llemacs-workspace-dir))
          (should (file-directory-p llemacs-source-dir))
          (should (file-directory-p llemacs-config-dir))
          (should (file-exists-p (expand-file-name ".env" llemacs-config-dir)))
          (should (file-exists-p (expand-file-name "github-token" llemacs-config-dir)))
          (should (llemacs-verify-installation)))
      (delete-directory temp-root t))))

;; Main verification function
(defun llemacs-verify-installation ()
  "Verify that all ELMO components are properly installed."
  (and (file-exists-p llemacs-work-dir)
       (file-exists-p llemacs-workspace-dir)
       (file-exists-p llemacs-source-dir)
       (file-exists-p llemacs-logs-dir)
       (file-exists-p llemacs-config-dir)
       (file-exists-p llemacs-github-token-file)))

(provide 'llemacs-verify-installation)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
