;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-28 09:53:32
;;; Time-stamp: <2024-12-28 09:53:32 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/llemacs/elisp/llemacs/06-llemacs-json-md.el

(require '02-llemacs-logging)
;; (require '03-llemacs--logging-utils)

(defcustom llemacs-format-script
  (expand-file-name "resources/scripts/json2md_or_md2json.sh" llemacs-path-workspace)
  "Path to JSON/Markdown conversion script.

Example:
  (setq llemacs-format-script \"/path/to/json2md.sh\")
  (file-exists-p llemacs-format-script) ;; Should return t"
  :type 'file
  :group 'llemacs)

(defun llemacs-json-to-markdown (json-path)
  "Convert JSON file at JSON-PATH to markdown using external script.

Example:
  (llemacs-json-to-markdown \"~/.emacs.d/llemacs/prompts/example.json\")
  ;; Creates example.md in same directory"
  (llemacs-check-json json-path)
  (condition-case err
      (call-process llemacs-format-script nil nil nil json-path)
    (error
     (llemacs--log-error (format "JSON to Markdown conversion failed: %s" err))
     nil)))

(defun llemacs-markdown-to-json (filepath-md)
  "Convert JSON file at FILEPATH to markdown using external script.
Example:
(llemacs-markdown-to-json \"~/.emacs.d/llemacs/prompts/example.md\")
;; Creates example.json in same directory"
  (if (not (string= (file-name-extension filepath-md) "md"))
      (progn
        (llemacs--log-error (format "Invalid file extension: %s" filepath-md))
        nil)
    (condition-case err
        (call-process llemacs-format-script nil nil nil (expand-file-name filepath-md))
      (error
       (llemacs--log-error (format "Markdown to JSON to conversion failed: %s" err))
       nil))))

(defun llemacs-load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments.
Example:
(llemacs-load-markdown-file \"~/.emacs.d/llemacs/prompts/example.md\")
;; => Returns markdown content as string, ignoring front-matter"
  (unless (file-exists-p file-path)
    (llemacs--log-error "File does not exist: %s" file-path))
  (condition-case err
      (let ((content (with-temp-buffer
                       (insert-file-contents file-path)
                       (goto-char (point-min))
                       (when (looking-at "<!--[^>]*-->")
                         (goto-char (match-end 0))
                         (forward-line))
                       (buffer-substring-no-properties (point) (point-max)))))
        (if (string-empty-p content)
            (llemacs--log-error (format "File is empty:\n%s" file-path))
          content))
    (error
     (llemacs--log-error (format "Failed to load markdown file\n%s\n%s" file-path err))
     nil)))

(defun llemacs-load-json-file (json-path)
  "Load JSON file at JSON-PATH by converting to markdown first.
Updates JSON if markdown exists and returns the markdown content."
  (let ((md-path (replace-regexp-in-string ".json" ".md" json-path)))
    (cond
     ((file-exists-p md-path)
      (llemacs-markdown-to-json md-path)
      (when (llemacs-check-json json-path)
        (llemacs-load-markdown-file md-path)))
     ((llemacs-check-json json-path)
      (llemacs-json-to-markdown json-path)
      (llemacs-load-markdown-file md-path)))))

(defun llemacs-check-json (json-path)
  "Validate JSON file at JSON-PATH using Python's json module.
Returns t if valid, nil otherwise."
  (if (not (file-exists-p json-path))
      (progn
        (llemacs--log-error (format "JSON file does not exist: %s" json-path))
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
        (llemacs--log-error
         (format "Invalid JSON file %s:\n%s"
                 json-path
                 (with-current-buffer temp-buffer
                   (buffer-string)))))
      (kill-buffer temp-buffer)
      (= exit-code 0))))

(provide '06-llemacs-json-md)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))