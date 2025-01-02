;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 18:03:29
;;; Time-stamp: <2025-01-02 18:03:29 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/loaders.el

;; ----------------------------------------
;; MD
;; ----------------------------------------
;; (defun llemacs--load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string, skipping metadata comments."
;;   (unless (file-exists-p file-path)
;;     (llemacs--logging-log-error "File does not exist: %s" file-path))
;;   (condition-case err
;;       (let ((content (with-temp-buffer
;;                        (insert-file-contents file-path)
;;                        (goto-char (point-min))
;;                        (when (looking-at "<!--[^>]*-->")
;;                          (goto-char (match-end 0))
;;                          (forward-line))
;;                        (buffer-substring-no-properties (point) (point-max)))))
;;         (if (string-empty-p content)
;;             (llemacs--logging-log-warn (format "File is empty:\n%s" file-path))
;;           content))
;;     (error
;;      (llemacs--logging-log-error (format "Failed to load markdown file\n%s\n%s" file-path err))
;;      nil)))


(defun llemacs--load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments."
  (unless (file-exists-p file-path)
    (llemacs--logging-log-error "File does not exist: %s" file-path))
  (condition-case err
      (let ((content (with-temp-buffer
                       (insert-file-contents file-path)
                       (goto-char (point-min))
                       (when (looking-at "<!--[^>]*-->")
                         (goto-char (match-end 0))
                         (forward-line))
                       (buffer-substring-no-properties (point) (point-max)))))
        (if (string-empty-p content)
            (progn
              (llemacs--logging-log-warn (format "File is empty:\n%s" file-path))
              "")
          content))
    (error
     (llemacs--logging-log-error (format "Failed to load markdown file\n%s\n%s" file-path err))
     nil)))

(defun llemacs--load-json-file (json-path)
  "Load JSON file at JSON-PATH by converting to markdown first."
  (let ((md-path (replace-regexp-in-string ".json" ".md" json-path)))
    (cond
     ((file-exists-p md-path)
      (llemacs--cvt-markdown-to-json md-path)
      (when (llemacs--check-json json-path)
        (llemacs--load-markdown-file md-path)))
     ((llemacs--check-json json-path)
      (llemacs--cvt-json-to-markdown json-path)
      (llemacs--load-markdown-file md-path)))))

;; ----------------------------------------
;; Helpers
;; ----------------------------------------
(defun llemacs--check-json (json-path)
  "Validate JSON file at JSON-PATH using Python's json module."
  (if (not (file-exists-p json-path))
      (progn
        (llemacs--logging-log-error (format "JSON file does not exist: %s" json-path))
        nil)
    (let* ((temp-buffer (generate-new-buffer "*json-check*"))
           (exit-code
            (call-process "python3" nil temp-buffer nil
                          "-c" "
import json,sys
try:
    json.load(open(sys.argv[1]))
except json.JSONDecodeError as e:
    print(f'Error at line {e.lineno}, column {e.colno}: {e.msg}')"
                          json-path)))
      (unless (= exit-code 0)
        (llemacs--logging-log-error
         (format "Invalid JSON file %s:\n%s"
                 json-path
                 (with-current-buffer temp-buffer
                   (buffer-string)))))
      (kill-buffer temp-buffer)
      (= exit-code 0))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))