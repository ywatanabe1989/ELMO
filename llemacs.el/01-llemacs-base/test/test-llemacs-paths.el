;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:43:20
;;; Time-stamp: <2025-01-04 14:43:20 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-paths.el

(require 'ert)
(require 'llemacs)

;; test-llemacs-paths.el:
;; (ert 'test-llemacs-sys-paths)
;; (ert 'test-llemacs-path-updates)
;; (ert 'test-llemacs-paths-init)
;; (ert 'test-llemacs-project-paths)
;; (ert 'test-llemacs-path-generation)
;; (ert 'test-llemacs-path-cleanup)

(ert-deftest test-llemacs-sys-paths ()
  "Test system path structure and permissions."
  (should (file-directory-p llemacs--path))
  (should (file-directory-p llemacs--path-workspace))
  (should (file-directory-p llemacs--path-agents))
  (should (file-directory-p llemacs--path-res))
  (should (file-directory-p llemacs--path-logs-sys))
  (should (file-writable-p llemacs--path-logs-all-sys))
  (should (file-writable-p llemacs--path-logs-debug-sys))
  (should (file-writable-p llemacs--path-logs-info-sys)))


(ert-deftest test-llemacs-path-updates ()
  "Test path updates on project switch."
  (let ((test-id-1 "005-test-1")
        (test-id-2 "005-test-2"))

    ;; Switch to first project
    (llemacs--pj-set-cur-pj test-id-1 t)
    (let ((path-1 llemacs--path-pj))

      ;; Switch to second project
      (llemacs--pj-set-cur-pj test-id-2 t)
      (should-not (equal path-1 llemacs--path-pj))

      ;; Verify paths updated
      (should (string-match-p test-id-2 llemacs--path-pj))
      (should (string-match-p test-id-2 llemacs--path-pj-logs)))))

(defun llemacs--path-cleanup-pj (pj-id)
  "Clean up all files and directories for project PJ-ID."
  (when (and pj-id (file-exists-p (llemacs--path-gen-pj-root pj-id)))
    (delete-directory (llemacs--path-gen-pj-root pj-id) t)))

(ert-deftest test-llemacs-paths-init ()
  "Test path initialization."

  ;; Base paths
  (should (file-directory-p llemacs--path-base))
  (should (file-directory-p llemacs--path-logs))
  (should (file-directory-p llemacs--path-projects))

  ;; System paths
  (should (file-directory-p llemacs--path-logs-sys))
  (should (file-exists-p llemacs--path-locks-sys)))

(ert-deftest test-llemacs-project-paths ()
  "Test project-specific paths."
  (let ((test-id "016-test"))
    (llemacs--pj-set-cur-pj test-id t)

    ;; Project root
    (should (file-directory-p llemacs--path-pj))

    ;; Project subdirectories
    (should (file-directory-p llemacs--path-pj-config))
    (should (file-directory-p llemacs--path-pj-logs))
    (should (file-directory-p llemacs--path-pj-data))

    ;; Project files
    (should (file-exists-p llemacs--path-pj-lock))
    (should (file-exists-p llemacs--path-pj-config-main))))

(ert-deftest test-llemacs-path-generation ()
  "Test path generation functions."
  (let ((test-id "017-test"))

    ;; Project paths
    (should (string-match-p test-id
                            (llemacs--path-gen-pj-root test-id)))
    (should (string-match-p "config"
                            (llemacs--path-gen-pj-config test-id)))

    ;; Log paths
    (should (string-match-p "logs/all"
                            (llemacs--path-gen-pj-logs-all test-id)))
    (should (string-match-p "logs/debug"
                            (llemacs--path-gen-pj-logs-debug test-id)))))

(ert-deftest test-llemacs-path-cleanup ()
  "Test path cleanup operations."
  (let ((test-id "018-test"))
    (llemacs--pj-set-cur-pj test-id t)

    ;; Create test files
    (write-region "test" nil
                  (expand-file-name "test.txt" llemacs--path-pj-data))

    ;; Cleanup
    (llemacs--path-cleanup-pj test-id)

    ;; Verify cleanup
    (should-not (file-exists-p llemacs--path-pj))))

(provide 'test-llemacs-paths)

;;; test-llemacs-paths.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))