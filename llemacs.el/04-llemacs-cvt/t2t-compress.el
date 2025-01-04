;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 14:37:14
;;; Time-stamp: <2025-01-01 14:37:14 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/04-llemacs-cvt/04-llemacs-cvt-compress.el

(defvar llemacs--cvt-compress-max-length 1024
  "Maximum length for compressed text output.")

(defun llemacs--cvt-compress-text (text &optional max-len)
  "Compress TEXT using LLM within MAX-LEN characters."
  (let ((limit (or max-len llemacs--cvt-compress-max-length)))
    (if (<= (length text) limit)
        text
      (llemacs-run
       (format "Compress to %d chars, preserve key information:\n%s"
               limit text)))))

;; (defun llemacs--cvt-compress-json (json-data &optional max-len)
;;   "Compress JSON-DATA using LLM summarization."
;;   (llemacs--cvt-compress-text (json-encode json-data) max-len))

;; (defun llemacs--cvt-compress-file (file &optional max-len)
;;   "Compress contents of FILE using LLM."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (llemacs--cvt-compress-text (buffer-string) max-len)))

;; (defun llemacs--cvt-compress-dir (dir pattern &optional max-len)
;;   "Compress files matching PATTERN in DIR."
;;   (let ((files (directory-files dir t pattern))
;;         (compressed-dir (expand-file-name "compressed" dir)))
;;     (make-directory compressed-dir t)
;;     (mapcar
;;      (lambda (file)
;;        (let ((compressed (llemacs--cvt-compress-file file max-len))
;;              (comp-file (expand-file-name
;;                          (concat "comp-" (file-name-nondirectory file))
;;                          compressed-dir)))
;;          (with-temp-file comp-file
;;            (insert compressed))
;;          comp-file))
;;      files)))

;; (defun llemacs--cvt-compress-batch (texts &optional max-len)
;;   "Compress multiple TEXTS using LLM."
;;   (mapcar (lambda (text)
;;             (llemacs--cvt-compress-text text max-len))
;;           texts))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))