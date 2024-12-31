;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:32:07
;;; Time-stamp: <2024-12-31 23:32:07 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/04-llemacs-conversion/04-llemacs-cvt-json-and-md.el

(require '02-llemacs-logging)

(defcustom llemacs--cvt-format-script
  (expand-file-name "04-llemacs-cvt-json-and-md.sh" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to JSON/Markdown conversion script."
  :type 'file
  :group 'llemacs)

;; ----------------------------------------
;; Converters
;; ----------------------------------------
(defun llemacs--cvt-json-to-markdown (json-path)
  "Convert JSON file at JSON-PATH to markdown using external script."
  (llemacs-check-json json-path)
  (condition-case err
      (call-process llemacs--cvt-format-script nil nil nil json-path)
    (error
     (llemacs--logging-error (format "JSON to Markdown conversion failed: %s" err))
     nil)))

(defun llemacs--cvt-markdown-to-json (filepath-md)
  "Convert JSON file at FILEPATH to markdown using external script."
  (if (not (string= (file-name-extension filepath-md) "md"))
      (progn
        (llemacs--logging-error (format "Invalid file extension: %s" filepath-md))
        nil)
    (condition-case err
        (call-process llemacs--cvt-format-script nil nil nil (expand-file-name filepath-md))
      (error
       (llemacs--logging-error (format "Markdown to JSON to conversion failed: %s" err))
       nil))))

;; ----------------------------------------
;; Loaders
;; ----------------------------------------
(defun llemacs-load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments."
  (unless (file-exists-p file-path)
    (llemacs--logging-error "File does not exist: %s" file-path))
  (condition-case err
      (let ((content (with-temp-buffer
                       (insert-file-contents file-path)
                       (goto-char (point-min))
                       (when (looking-at "<!--[^>]*-->")
                         (goto-char (match-end 0))
                         (forward-line))
                       (buffer-substring-no-properties (point) (point-max)))))
        (if (string-empty-p content)
            (llemacs--logging-error (format "File is empty:\n%s" file-path))
          content))
    (error
     (llemacs--logging-error (format "Failed to load markdown file\n%s\n%s" file-path err))
     nil)))

(defun llemacs-load-json-file (json-path)
  "Load JSON file at JSON-PATH by converting to markdown first."
  (let ((md-path (replace-regexp-in-string ".json" ".md" json-path)))
    (cond
     ((file-exists-p md-path)
      (llemacs--cvt-markdown-to-json md-path)
      (when (llemacs-check-json json-path)
        (llemacs-load-markdown-file md-path)))
     ((llemacs-check-json json-path)
      (llemacs--cvt-json-to-markdown json-path)
      (llemacs-load-markdown-file md-path)))))

;; ----------------------------------------
;; Helpers
;; ----------------------------------------
(defun llemacs-check-json (json-path)
  "Validate JSON file at JSON-PATH using Python's json module."
  (if (not (file-exists-p json-path))
      (progn
        (llemacs--logging-error (format "JSON file does not exist: %s" json-path))
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
        (llemacs--logging-error
         (format "Invalid JSON file %s:\n%s"
                 json-path
                 (with-current-buffer temp-buffer
                   (buffer-string)))))
      (kill-buffer temp-buffer)
      (= exit-code 0))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))