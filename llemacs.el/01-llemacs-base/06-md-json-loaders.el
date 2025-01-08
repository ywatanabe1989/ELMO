;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 06:57:40
;;; Timestamp: <2025-01-08 06:57:40>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/06-md-json-loaders.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'yaml)

(defun llemacs--load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments."
  (unless (file-exists-p file-path)
    (llemacs--logging-write-error-pj "File does not exist: %s" file-path))
  (condition-case err
      (let ((content (with-temp-buffer
                       (insert-file-contents file-path)
                       (goto-char (point-min))
                       (while (re-search-forward "<!--[^>]*-->" nil t)
                         (replace-match ""))
                       (buffer-substring-no-properties (point-min) (point-max)))))
        (if (string-empty-p content)
            (progn
              (llemacs--logging-write-warn-pj (format "File is empty:\n%s" file-path))
              "")
          content))
    (error
     (llemacs--logging-write-error-pj (format "Failed to load markdown file\n%s\n%s" file-path err))
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

(defun llemacs--load-yaml-file (file)
  "Load and parse YAML file content."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((yaml-str (buffer-string)))
      (yaml-parse-string yaml-str :object-type 'alist :sequence-type 'list))))

;; ----------------------------------------
;; Helpers
;; ----------------------------------------
(defun llemacs--check-json (json-path)
  "Validate JSON file at JSON-PATH using Python's json module."
  (if (not (file-exists-p json-path))
      (progn
        (llemacs--logging-write-error-pj (format "JSON file does not exist: %s" json-path))
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
        (llemacs--logging-write-error-pj
         (format "Invalid JSON file %s:\n%s"
                 json-path
                 (with-current-buffer temp-buffer
                   (buffer-string)))))
      (kill-buffer temp-buffer)
      (= exit-code 0))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
