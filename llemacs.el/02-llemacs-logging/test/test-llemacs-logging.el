;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 16:03:21
;;; Time-stamp: <2025-01-04 16:03:21 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test/test-llemacs-logging.el

;; For interactive testing
(defun llemacs--loggging-test-loggers ()
  (interactive)
  (message "Testing logger...")
  (message "llemacs--log-levels-sys: %S" llemacs--log-levels-sys)
  (llemacs--logging-define-loggers-sys)
  (llemacs--logging-define-loggers-pj)

  ;; Test actual logging
  (message "Testing error logging...")
  (llemacs--logging-write-error-sys "Test error message")
  (message "Testing debug logging...")
  (llemacs--logging-write-debug-sys "Test debug message"))

;; (llemacs--loggging-test-loggers)

;; Setup helper
(defmacro with-test-logging (&rest body)
  `(let* ((temp-dir (make-temp-file "test-logs-" t))
          (llemacs--path-logs-sys temp-dir)
          (llemacs--path-logs-pj (expand-file-name "pj" temp-dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

;; Test cases
(ert-deftest test-llemacs-logging-basic ()
  (with-test-logging
   ;; Test system logging
   (llemacs--logging-write 'error "Test error" nil)
   (should (file-exists-p (expand-file-name "error.log" llemacs--path-logs-sys)))

   ;; Test project logging
   (let ((llemacs--cur-pj "test-project"))
     (llemacs--logging-write 'info "Test info" t)
     (should (file-exists-p (expand-file-name "info.log" llemacs--path-logs-pj))))))

(ert-deftest test-llemacs-logging-level ()
  "Test log level functionality."
  (should (= (llemacs--logging-get-level-value 'error) 3))
  (should (= (llemacs--logging-get-level-value 'warn) 2))
  (should (= (llemacs--logging-get-level-value 'info) 1))
  (should (= (llemacs--logging-get-level-value 'debug) 0))
  (should (llemacs--logging-should-log-p 'error))
  (let ((llemacs--logging-level-threshold 'warn))
    (should-not (llemacs--logging-should-log-p 'info))))

(ert-deftest test-llemacs-logging-format ()
  "Test log message formatting."
  (let* ((test-msg "Test message")
         (formatted (llemacs--logging-format-message 'error test-msg)))
    (should (string-match-p (regexp-quote test-msg) formatted))
    (should (string-match-p "\\[ERROR LOG\\]" formatted))
    (should (string-match-p (regexp-quote llemacs--logging-splitter) formatted))))

(ert-deftest test-llemacs-logging-write ()
  "Test log writing functionality."
  (let* ((temp-dir (make-temp-file "test-logs-" t))
         (llemacs--path-logs-all-sys (expand-file-name "all.log" temp-dir))
         (llemacs--path-logs-error-sys (expand-file-name "error.log" temp-dir)))
    (unwind-protect
        (progn
          (llemacs--logging-write 'error "Test error message")
          (should (file-exists-p llemacs--path-logs-all-sys))
          (should (file-exists-p llemacs--path-logs-error-sys))
          (should (string-match-p "Test error message"
                                  (with-temp-buffer
                                    (insert-file-contents llemacs--path-logs-error-sys)
                                    (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-logging-view ()
  "Test log viewing functionality."
  (let* ((temp-dir (make-temp-file "test-logs-" t))
         (llemacs--path-logs-all-sys (expand-file-name "all.log" temp-dir))
         (llemacs--buf-logging-sys "*TEST-LOGGING*"))
    (unwind-protect
        (progn
          (with-temp-file llemacs--path-logs-all-sys
            (insert "Test log entry\n"))
          (llemacs--logging-view nil nil)
          (with-current-buffer llemacs--buf-logging-sys
            (should (string-match-p "Test log entry" (buffer-string)))))
      (delete-directory temp-dir t))))

(ert-deftest test-llemacs-logging-maintenance ()
  "Test log maintenance functions."
  (let* ((temp-dir (make-temp-file "test-logs-" t))
         (llemacs--path-logs-backup-sys (expand-file-name "backup" temp-dir)))
    (unwind-protect
        (progn
          (make-directory llemacs--path-logs-backup-sys t)
          (llemacs--logging-backup-files)
          (should (file-exists-p llemacs--path-logs-backup-sys)))
      (delete-directory temp-dir t))))

(provide 'test-llemacs-logging)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))