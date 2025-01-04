;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:24:55
;;; Time-stamp: <2025-01-04 14:24:55 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test-llemacs-loaders.el

(require 'ert)
(require 'llemacs)

;; (ert 'test-llemacs-markdown-loader)
;; (ert 'test-llemacs-json-checker)
;; (ert 'test-llemacs-json-loader)


(ert-deftest test-llemacs-markdown-loader ()
  "Test markdown file loading functionality."

  ;; Test with metadata
  (let ((test-file (make-temp-file "test-md")))
    (with-temp-file test-file
      (insert "<!-- metadata -->\ntest content"))
    (should (string= "test content"
                     (string-trim (llemacs--load-markdown-file test-file))))
    (delete-file test-file))

  ;; Test without metadata
  (let ((test-file (make-temp-file "test-md-plain")))
    (with-temp-file test-file
      (insert "plain content"))
    (should (string= "plain content"
                     (llemacs--load-markdown-file test-file)))
    (delete-file test-file))

  ;; Test empty file
  (let ((test-file (make-temp-file "test-md-empty")))
    (should (string= "" (llemacs--load-markdown-file test-file)))
    (delete-file test-file))

  ;; Test nonexistent file
  (should-not (llemacs--load-markdown-file "nonexistent.md")))


(defun llemacs--check-json (file)
  "Check if FILE contains valid JSON."
  (when (file-exists-p file)
    (condition-case nil
        (progn
          (json-read-file file)
          t)
      (error nil))))

(ert-deftest test-llemacs-json-checker ()
  "Test JSON validation functionality."

  ;; Valid JSON
  (let ((test-file (make-temp-file "test-json")))
    (with-temp-file test-file
      (insert "{\"key\": \"value\"}"))
    (should (llemacs--check-json test-file))
    (delete-file test-file))

  ;; Invalid JSON
  (let ((test-file (make-temp-file "test-json-invalid")))
    (with-temp-file test-file
      (insert "{invalid json}"))
    (should-not (llemacs--check-json test-file))
    (delete-file test-file))

  ;; Nonexistent file
  (should-not (llemacs--check-json "nonexistent.json")))

;; (ert test-llemacs-json-checker)

(ert-deftest test-llemacs-json-loader ()
  "Test JSON/Markdown conversion and loading."

  ;; Test with existing markdown
  (let ((json-file (make-temp-file "test-json"))
        (md-file (make-temp-file "test-md")))
    (with-temp-file json-file
      (insert "{\"key\": \"value\"}"))
    (with-temp-file md-file
      (insert "test content"))
    (should (llemacs--load-json-file json-file))
    (delete-file json-file)
    (delete-file md-file))

  ;; Test with only JSON
  (let ((json-file (make-temp-file "test-json-only")))
    (with-temp-file json-file
      (insert "{\"key\": \"value\"}"))
    (should (llemacs--load-json-file json-file))
    (delete-file json-file)))

(provide 'test-llemacs-loaders)

;;; test-llemacs-loaders.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))