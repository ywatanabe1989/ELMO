;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:23:46
;;; Time-stamp: <2025-01-04 14:23:46 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-logging.el

;; test-llemacs-logging.el:
;; (ert 'test-llemacs-log-basics)
;; (ert 'test-llemacs-log-system)
;; (ert 'test-llemacs-log-formatting)
;; (ert 'test-llemacs-log-rotation)


(require 'ert)
(require 'llemacs)

(ert-deftest test-llemacs-log-basics ()
  "Test basic logging functionality."
  (let ((test-id "013-test"))
    (llemacs--cur-pj-set test-id t)

    ;; Test different log levels
    (llemacs--log-debug "Debug message")
    (llemacs--log-info "Info message")
    (llemacs--log-warn "Warning message")
    (llemacs--log-error "Error message")

    ;; Verify log files exist and are writable
    (should (file-exists-p llemacs--path-pj-logs-all))
    (should (file-exists-p llemacs--path-pj-logs-debug))
    (should (file-exists-p llemacs--path-pj-logs-info))
    (should (file-exists-p llemacs--path-pj-logs-warn))
    (should (file-exists-p llemacs--path-pj-logs-error))))

(ert-deftest test-llemacs-log-system ()
  "Test system-level logging."

  ;; System logs
  (llemacs--log-sys-debug "System debug")
  (llemacs--log-sys-info "System info")

  ;; Verify system log files
  (should (file-exists-p llemacs--path-logs-all-sys))
  (should (file-exists-p llemacs--path-logs-debug-sys))
  (should (file-exists-p llemacs--path-logs-info-sys)))

(ert-deftest test-llemacs-log-formatting ()
  "Test log message formatting."
  (let ((test-id "014-test"))
    (llemacs--cur-pj-set test-id t)

    ;; Basic format
    (let ((msg (llemacs--log-format "test" "DEBUG")))
      (should (string-match-p "\\[DEBUG\\]" msg))
      (should (string-match-p test-id msg)))

    ;; With context
    (let ((msg (llemacs--log-format "test" "INFO" "context")))
      (should (string-match-p "\\[INFO\\]" msg))
      (should (string-match-p "context" msg)))))

(ert-deftest test-llemacs-log-rotation ()
  "Test log file rotation."
  (let ((test-id "015-test"))
    (llemacs--cur-pj-set test-id t)

    ;; Generate large log
    (dotimes (i 1000)
      (llemacs--log-debug (format "Test message %d" i)))

    ;; Check rotation
    (should (file-exists-p
             (concat llemacs--path-pj-logs-debug ".1")))

    ;; Verify current log
    (should (file-exists-p llemacs--path-pj-logs-debug))))

(provide 'test-llemacs-logging)

;;; test-llemacs-logging.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))