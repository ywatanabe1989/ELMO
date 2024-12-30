;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-30 17:21:18
;;; Time-stamp: <2024-12-30 17:21:18 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/10-llemacs-compressor.el

(require '01-llemacs-config)
(require '10-llemacs-run)

(defvar llemacs-compress-max-length 1024
  "Maximum length for compressed text output.")

(defun llemacs-compress-text (text &optional max-len)
  "Compress TEXT using LLM within MAX-LEN characters."
  (let ((limit (or max-len llemacs-compress-max-length)))
    (if (<= (length text) limit)
        text
      (llemacs-run
       (format "Compress to %d chars, preserve key information:\n%s"
               limit text)))))

;; (defun llemacs-compress-json (json-data &optional max-len)
;;   "Compress JSON-DATA using LLM summarization."
;;   (llemacs-compress-text (json-encode json-data) max-len))

;; (defun llemacs-compress-file (file &optional max-len)
;;   "Compress contents of FILE using LLM."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (llemacs-compress-text (buffer-string) max-len)))

;; (defun llemacs-compress-dir (dir pattern &optional max-len)
;;   "Compress files matching PATTERN in DIR."
;;   (let ((files (directory-files dir t pattern))
;;         (compressed-dir (expand-file-name "compressed" dir)))
;;     (make-directory compressed-dir t)
;;     (mapcar
;;      (lambda (file)
;;        (let ((compressed (llemacs-compress-file file max-len))
;;              (comp-file (expand-file-name
;;                          (concat "comp-" (file-name-nondirectory file))
;;                          compressed-dir)))
;;          (with-temp-file comp-file
;;            (insert compressed))
;;          comp-file))
;;      files)))

;; (defun llemacs-compress-batch (texts &optional max-len)
;;   "Compress multiple TEXTS using LLM."
;;   (mapcar (lambda (text)
;;             (llemacs-compress-text text max-len))
;;           texts))

(provide '10-llemacs-compressor)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))