;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:24:18
;;; Time-stamp: <2025-01-04 14:24:18 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-base.el

(require 'ert)
(require 'llemacs)

;; test-llemacs-base.el:
;; Evaluate: C-x C-e after each:
;; (ert 'test-llemacs-list)
;; (ert 'test-llemacs-buf-disp)
;; (ert 'test-llemacs-paths)
;; (ert 'test-llemacs-project-locks)
;; (ert 'test-llemacs-log-paths)
;; (ert 'test-llemacs-timestamp)

;; Test list functionality
(ert-deftest test-llemacs-list ()
  "Test llemacs symbol listing functions."
  (let ((commands (llemacs-list "command" "^llemacs-"))
        (variables (llemacs-list "variable" "^llemacs-"))
        (functions (llemacs-list "function" "^llemacs-"))
        (groups (llemacs-list "group" "^llemacs-")))
    (should (listp commands))
    (should (> (length commands) 0))
    (should (listp variables))
    (should (> (length variables) 0))
    (should (listp functions))
    (should (> (length functions) 0))
    (should (listp groups))
    (should (> (length groups) 0))))

;; Test buffer display
(ert-deftest test-llemacs-buf-disp ()
  "Test buffer display functions."
  (let ((test-buf "*test-llemacs*"))
    (llemacs--buf-disp test-buf nil t t t)
    (should (get-buffer test-buf))
    (should (eq 'org-mode
                (with-current-buffer test-buf major-mode)))
    (kill-buffer test-buf)))

;; (ert 'test-llemacs-buf-disp)

;; Test project paths
(ert-deftest test-llemacs-paths ()
  "Test project path management."
  (let ((test-id "001-test"))
    (should-error (llemacs--pj-set-cur-pj "invalid-id"))
    (llemacs--pj-set-cur-pj test-id t)
    (should (string= test-id (llemacs--pj-get-cur-pj)))
    (should (file-exists-p llemacs--path-pj))
    (should (file-exists-p llemacs--path-pj-logs))))

;; (ert 'test-llemacs-paths)

;; Test project locking
(ert-deftest test-llemacs-project-locks ()
  "Test project locking mechanism."
  (let ((test-id "002-test"))
    (llemacs--pj-set-cur-pj test-id t)
    (should (llemacs--pj-lock-acquire test-id))
    (should (llemacs--pj-lock-check test-id))
    (llemacs--pj-lock-release test-id)
    (should-not (llemacs--pj-lock-check test-id))))

;; (ert 'test-llemacs-project-locks)

;; Test log paths
(ert-deftest test-llemacs-log-paths ()
  "Test log path creation and validation."
  (let ((test-id "003-test"))
    (llemacs--pj-set-cur-pj test-id t)
    (should (file-exists-p llemacs--path-pj-logs-info))
    (should (file-exists-p llemacs--path-pj-logs-error))
    (should (file-exists-p llemacs--path-pj-logs-debug))))

;; (ert 'test-llemacs-log-paths)

;; Test timestamp
(ert-deftest test-llemacs-timestamp ()
  "Test timestamp functions."
  (let ((ts (llemacs-timestamp-get)))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}$" ts))))

;; (ert 'test-llemacs-timestamp)

(provide 'test-llemacs-base)

;;; test-llemacs-base.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))