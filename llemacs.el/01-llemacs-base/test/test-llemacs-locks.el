;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:23:39
;;; Time-stamp: <2025-01-04 14:23:39 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-locks.el

(require 'ert)

;; test-llemacs-locks.el:
;; (ert 'test-llemacs-lock-basic)
;; (ert 'test-llemacs-lock-force-release)
;; (ert 'test-llemacs-lock-cleanup)
;; (ert 'test-llemacs-lock-stale)


;; Setup helper
(defmacro with-test-lock (&rest body)
  `(let* ((temp-dir (make-temp-file "test-locks-" t))
          (llemacs--path-projects temp-dir))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;; Test cases
(ert-deftest test-llemacs-lock-basic ()
  "Test basic lock operations"
  (with-test-lock
   (let ((test-id "test-project"))
     ;; Test lock acquisition
     (should (llemacs--pj-lock-acquire test-id))
     (should (llemacs--pj-lock-check test-id))

     ;; Test lock release
     (llemacs--pj-lock-release test-id)
     (should-not (llemacs--pj-lock-check test-id)))))

(ert-deftest test-llemacs-lock-force-release ()
  "Test force release functionality"
  (with-test-lock
   (let ((test-id "test-project"))
     (llemacs--pj-lock-acquire test-id)
     (should (llemacs--pj-lock-check test-id))
     (llemacs--pj-lock-force-release test-id)
     (should-not (llemacs--pj-lock-check test-id)))))

(ert-deftest test-llemacs-lock-cleanup ()
  "Test lock cleanup functionality"
  (with-test-lock
   (let ((test-id "test-project"))
     ;; Create lock
     (llemacs--pj-lock-acquire test-id)
     (should (llemacs--pj-lock-check test-id))

     ;; Run cleanup
     (llemacs--pj-lock-cleanup)
     (should-not (llemacs--pj-lock-check test-id)))))

(ert-deftest test-llemacs-lock-stale ()
  "Test stale lock detection"
  (with-test-lock
   (let ((test-id "test-project"))
     (llemacs--pj-lock-acquire test-id)
     (should (llemacs--pj-lock-check-stale test-id))
     (llemacs--pj-lock-release test-id))))

(provide 'test-llemacs-lock-system)

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2025-01-04 12:00:08
;; ;;; Time-stamp: <2025-01-04 12:00:08 (ywatanabe)>
;; ;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test-llemacs-locks.el


;; ;;; test-llemacs-locks.el --- Project locking tests -*- lexical-binding: t -*-

;; (require 'ert)
;; (require 'llemacs)

;; (ert-deftest test-llemacs-lock-basics ()
;;   "Test basic locking operations."
;;   (let ((test-id "006-test"))

;;     ;; Initial state
;;     (should-not (llemacs--pj-lock-check test-id))

;;     ;; Acquire lock
;;     (should (llemacs--pj-lock-acquire test-id))
;;     (should (llemacs--pj-lock-check test-id))

;;     ;; Lock info format
;;     (let ((lock-info (llemacs--pj-lock-check test-id)))
;;       (should (string-match-p "@" lock-info))
;;       (should (string-match-p (user-login-name) lock-info)))

;;     ;; Release lock
;;     (llemacs--pj-lock-release test-id)
;;     (should-not (llemacs--pj-lock-check test-id))))

;; (ert-deftest test-llemacs-lock-conflicts ()
;;   "Test lock conflict handling."
;;   (let ((test-id "007-test"))

;;     ;; First acquisition succeeds
;;     (should (llemacs--pj-lock-acquire test-id))

;;     ;; Second acquisition fails
;;     (should-not (llemacs--pj-lock-acquire test-id))

;;     ;; Cleanup
;;     (llemacs--pj-lock-release test-id)))

;; (ert-deftest test-llemacs-lock-project-switch ()
;;   "Test locking during project switching."
;;   (let ((test-id-1 "008-test-1")
;;         (test-id-2 "008-test-2"))

;;     ;; Switch to first project
;;     (llemacs--pj-set-cur-pj test-id-1)
;;     (should (llemacs--pj-lock-check test-id-1))

;;     ;; Switch to second project should release first lock
;;     (llemacs--pj-set-cur-pj test-id-2)
;;     (should-not (llemacs--pj-lock-check test-id-1))
;;     (should (llemacs--pj-lock-check test-id-2))))

;; (ert-deftest test-llemacs-lock-cleanup ()
;;   "Test lock cleanup operations."
;;   (let ((test-id "009-test"))

;;     ;; Create lock
;;     (llemacs--pj-lock-acquire test-id)

;;     ;; Force cleanup
;;     (llemacs--pj-lock-cleanup-all)

;;     ;; Check stale detection
;;     (should (llemacs--pj-lock-check-stale test-id))

;;     ;; Cleanup
;;     (llemacs--pj-lock-release test-id)))

;; (provide 'test-llemacs-locks)

;; ;;; test-llemacs-locks.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))