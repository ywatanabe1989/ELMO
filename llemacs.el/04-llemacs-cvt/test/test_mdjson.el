;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:36:42
;;; Time-stamp: <2025-01-04 14:36:42 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/test/test_mdjson.el

(ert-deftest test-llemacs-cvt-mdjson ()
  "Test markdown/JSON conversion."
  (let ((llemacs--cvt-format-script "echo")
        (llemacs--path-python-env-sys "/tmp/")
        (llemacs--logging-write-error-sys (lambda (msg) nil)))
    (let ((md-file (make-temp-file "test" nil ".md"))
          (json-file (make-temp-file "test" nil ".json")))
      (with-temp-file md-file
        (insert "# Test\nContent"))
      (with-temp-file json-file
        (insert "{\"key\": \"value\"}"))

      ;; Test conversion
      (should (= 0 (llemacs--cvt-mdjson md-file)))
      (should (= 0 (llemacs--cvt-mdjson json-file)))

      ;; Test invalid file
      (should (= 0 (llemacs--cvt-mdjson "nonexistent.md")))

      ;; Cleanup
      (delete-file md-file)
      (delete-file json-file))))

;; For testing:
;; (ert 'test-llemacs-cvt-mdjson)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))