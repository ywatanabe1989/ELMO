;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 12:39:15
;;; Time-stamp: <2025-01-04 12:39:15 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test/test-llemacs-logging-getters.el

(ert-deftest test-llemacs-logging-get-log-entries ()
  "Test log entry retrieval from file."
  (let ((temp-file (make-temp-file "test-log")))
    (unwind-protect
        (progn
          ;; Test empty file
          (write-region "" nil temp-file)
          (should (equal (llemacs--logging-get-log-entries temp-file) nil))

          ;; Test single entry
          (write-region "test entry\n" nil temp-file)
          (should (equal (llemacs--logging-get-log-entries temp-file)
                         '("test entry")))

          ;; Test multiple entries
          (write-region "entry1\nentry2\nentry3\n" nil temp-file)
          (should (equal (llemacs--logging-get-log-entries temp-file)
                         '("entry1" "entry2" "entry3"))))
      (delete-file temp-file))))

(ert-deftest test-llemacs-logging-get-logs ()
  "Test log retrieval functionality."
  (let* ((temp-dir (make-temp-file "test-logs-" t))
         (llemacs--path-logs-all-sys (expand-file-name "all.log" temp-dir))
         (llemacs--path-logs-info-sys (expand-file-name "info.log" temp-dir))
         (llemacs--path-pj-logs-all (expand-file-name "pj-all.log" temp-dir)))
    (unwind-protect
        (progn
          (write-region "sys-all\n" nil llemacs--path-logs-all-sys)
          (write-region "sys-info\n" nil llemacs--path-logs-info-sys)
          (write-region "pj-all\n" nil llemacs--path-pj-logs-all)

          (should (equal (llemacs--logging-get-logs nil nil) '("sys-all")))
          (should (equal (llemacs--logging-get-logs nil t) '("pj-all")))
          (should (equal (llemacs--logging-get-logs 'info nil) '("sys-info"))))
      (delete-directory temp-dir t))))

(provide 'test-llemacs-logging-getters)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))