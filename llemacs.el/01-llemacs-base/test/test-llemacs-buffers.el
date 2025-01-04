;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:24:31
;;; Time-stamp: <2025-01-04 14:24:31 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-buffers.el

(require 'ert)
(require 'llemacs)

;; (ert 'test-llemacs-buffer-naming)
;; (ert 'test-llemacs-buffer-display)
;; (ert 'test-llemacs-buffer-updates)
;; (ert 'test-llemacs-buffer-display-handlers)


(ert-deftest test-llemacs-buffer-naming ()
  "Test buffer naming conventions."
  (let ((test-id "010-test"))
    (llemacs--cur-pj-set test-id t)

    ;; System buffers
    (should (string= "*LLEMACS-MAIN*" llemacs--buf-main-sys))
    (should (string= "*LLEMACS-LOGGING*" llemacs--buf-logging-sys))

    ;; Project buffers
    (should (string-match-p test-id llemacs--buf-main-pj))
    (should (string-match-p test-id llemacs--buf-logging-pj))
    (should (string-match-p test-id llemacs--buf-debug-pj))))

;; (ert test-llemacs-buffer-naming)

(ert-deftest test-llemacs-buffer-display ()
  "Test buffer display functionality."

  ;; Basic display
  (llemacs--buf-disp "*test-basic*" nil t t nil)
  (should (get-buffer "*test-basic*"))
  (should (get-buffer-window "*test-basic*"))

  ;; With org mode
  (llemacs--buf-disp "*test-org*" nil t t t)
  (with-current-buffer "*test-org*"
    (should (eq major-mode 'org-mode)))

  ;; With content
  (let ((test-file (make-temp-file "test-content")))
    (with-temp-file test-file
      (insert "test content"))
    (llemacs--buf-disp "*test-content*" test-file t t nil)
    (with-current-buffer "*test-content*"
      (should (string= (buffer-string) "test content")))
    (delete-file test-file))

  ;; Cleanup
  (kill-buffer "*test-basic*")
  (kill-buffer "*test-org*")
  (kill-buffer "*test-content*"))

(ert-deftest test-llemacs-buffer-updates ()
  "Test buffer name updates on project switch."
  (let ((test-id-1 "011-test-1")
        (test-id-2 "011-test-2"))

    ;; Switch projects and verify buffer name updates
    (llemacs--cur-pj-set test-id-1 t)
    (let ((main-1 llemacs--buf-main-pj)
          (log-1 llemacs--buf-logging-pj))

      (llemacs--cur-pj-set test-id-2 t)
      (should-not (string= main-1 llemacs--buf-main-pj))
      (should-not (string= log-1 llemacs--buf-logging-pj))

      (should (string-match-p test-id-2 llemacs--buf-main-pj))
      (should (string-match-p test-id-2 llemacs--buf-logging-pj)))))

(ert-deftest test-llemacs-buffer-display-handlers ()
  "Test specialized buffer display handlers."
  (let ((test-id "012-test"))
    (llemacs--cur-pj-set test-id t)

    ;; System buffers
    (llemacs--buf-disp-main-sys)
    (should (get-buffer llemacs--buf-main-sys))

    (llemacs--buf-disp-logging-sys)
    (should (get-buffer llemacs--buf-logging-sys))

    ;; Project buffers
    (llemacs--buf-disp-main-pj)
    (should (get-buffer llemacs--buf-main-pj))

    (llemacs--buf-disp-logging-pj)
    (should (get-buffer llemacs--buf-logging-pj))))

(provide 'test-llemacs-buffers)

;;; test-llemacs-buffers.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))