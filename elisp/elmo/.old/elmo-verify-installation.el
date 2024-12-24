;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:44:15
;;; Time-stamp: <2024-12-02 06:44:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-verify-installation.el


(require 'elmo-config)
(require 'ert)

(ert-deftest test-elmo-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (elmo-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (elmo-check-dependencies))))

(ert-deftest test-elmo-create-directories ()
  "Test directory creation."
  (let ((elmo-work-dir (make-temp-file "elmo-test-" t))
        (elmo-workspace-dir (make-temp-file "elmo-workspace-" t))
        (elmo-source-dir (make-temp-file "elmo-source-" t)))
    (unwind-protect
        (progn
          (elmo-create-directories)
          (should (file-directory-p elmo-work-dir))
          (should (file-directory-p elmo-workspace-dir))
          (should (file-directory-p elmo-source-dir)))
      (delete-directory elmo-work-dir t)
      (delete-directory elmo-workspace-dir t)
      (delete-directory elmo-source-dir t))))

(ert-deftest test-elmo-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "elmo-test-" t))
         (elmo-user-request-file (expand-file-name "user-request.md" temp-dir))
         (elmo-request-file (expand-file-name "request.md" temp-dir))
         (elmo-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (elmo-create-initial-files)
          (should (file-exists-p elmo-user-request-file))
          (should (file-exists-p elmo-request-file))
          (should (file-exists-p elmo-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-elmo-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "elmo-test-" t))
         (elmo-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (elmo-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "ELMO_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-elmo-verify-installation ()
  "Test installation verification."
  (let* ((temp-dir (make-temp-file "elmo-test-" t))
         (elmo-work-dir temp-dir)
         (elmo-workspace-dir (expand-file-name "workspace" temp-dir))
         (elmo-source-dir (expand-file-name "source" temp-dir))
         (elmo-logs-dir (expand-file-name "logs" temp-dir))
         (elmo-config-dir (expand-file-name "config" temp-dir))
         (elmo-github-token-file (expand-file-name "token" temp-dir))
         (elmo-user-request-file (expand-file-name "user-request.md" temp-dir))
         (elmo-request-file (expand-file-name "request.md" temp-dir))
         (elmo-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (mapc (lambda (dir) (make-directory dir t))
                (list elmo-workspace-dir elmo-source-dir elmo-logs-dir elmo-config-dir))
          (mapc (lambda (file) (with-temp-file file (insert "test")))
                (list elmo-github-token-file elmo-user-request-file elmo-request-file elmo-history-file))
          (should (progn (elmo-verify-installation) t)))
      (delete-directory temp-dir t))))

(ert-deftest test-elmo-setup-github-token ()
  "Test GitHub token setup."
  (let* ((temp-dir (make-temp-file "elmo-test-" t))
         (elmo-github-token-file (expand-file-name "github-token" temp-dir))
         (test-token "ghp_test1234567890"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _) test-token)))
            (elmo-setup-github-token)
            (should (file-exists-p elmo-github-token-file))
            (should (string= (with-temp-buffer
                             (insert-file-contents elmo-github-token-file)
                             (string-trim (buffer-string)))
                           test-token))))
      (delete-directory temp-dir t))))

(ert-deftest test-elmo-full-installation-process ()
  "Integration test for the full installation process."
  (let* ((temp-root (make-temp-file "elmo-test-root-" t))
         (elmo-work-dir (expand-file-name "elmo" temp-root))
         (elmo-workspace-dir (expand-file-name "workspace" elmo-work-dir))
         (elmo-source-dir (expand-file-name "source" elmo-work-dir))
         (elmo-config-dir (expand-file-name "config" elmo-work-dir))
         (test-token "ghp_testtoken12345"))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) test-token))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) t)))
          (elmo-install)
          (should (file-directory-p elmo-work-dir))
          (should (file-directory-p elmo-workspace-dir))
          (should (file-directory-p elmo-source-dir))
          (should (file-directory-p elmo-config-dir))
          (should (file-exists-p (expand-file-name ".env" elmo-config-dir)))
          (should (file-exists-p (expand-file-name "github-token" elmo-config-dir)))
          (should (elmo-verify-installation)))
      (delete-directory temp-root t))))

;; Main verification function
(defun elmo-verify-installation ()
  "Verify that all ELMO components are properly installed."
  (and (file-exists-p elmo-work-dir)
       (file-exists-p elmo-workspace-dir)
       (file-exists-p elmo-source-dir)
       (file-exists-p elmo-logs-dir)
       (file-exists-p elmo-config-dir)
       (file-exists-p elmo-github-token-file)))

(provide 'elmo-verify-installation)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
