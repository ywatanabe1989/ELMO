;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 23:25:09
;;; Time-stamp: <2024-12-24 23:25:09 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/06-elmo-json-md.el

(defcustom elmo-format-script
  (expand-file-name "resources/scripts/json2md.sh" elmo-workspace-dir)
  "Path to JSON/Markdown conversion script."
  :type 'file
  :group 'elmo)

(defun elmo-json-to-markdown (filepath)
  "Convert JSON file at FILEPATH to markdown using external script."
  (condition-case err
      (call-process elmo-format-script nil nil nil filepath)
    (error
     (elmo-log-error (format "JSON to Markdown conversion failed: %s" err))
     nil)))

(defun elmo-md-to-json (filepath)
  "Convert JSON file at FILEPATH to markdown using external script."
  (condition-case err
      (call-process elmo-format-script nil nil nil filepath)
    (error
     (elmo-log-error (format "Markdown to JSON to conversion failed: %s" err))
     nil)))

;; (defun elmo-load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string, skipping metadata comments."
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (goto-char (point-min))
;;         (when (looking-at "<!--[^>]*-->")
;;           (goto-char (match-end 0))
;;           (forward-line))
;;         (buffer-substring-no-properties (point) (point-max)))
;;     (error
;;      (elmo-log-message (format "Failed to load markdown file %s: %s" file-path err))
;;      nil)))


(defun elmo-load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (when (looking-at "<!--[^>]*-->")
          (goto-char (match-end 0))
          (forward-line))
        (buffer-substring-no-properties (point) (point-max)))
    (error
     (elmo-log-error (format "Failed to load markdown file %s: %s" file-path err))
     nil)))

(defun elmo-load-json-file (json-path)
  "Load JSON file at JSON-PATH by converting to markdown first."
  (let ((md-path (concat (file-name-sans-extension json-path) ".md")))
    (when (elmo-json-to-markdown json-path)
      (elmo-load-markdown-file md-path))))

(provide '06-elmo-json-md)

;; ;; working
;; (elmo-json-to-markdown "~/.emacs.d/lisp/elmo/workspace/resources/prompts/001-context-to-elisp-code.json")
;; (elmo-load-markdown-file "~/.emacs.d/lisp/elmo/workspace/resources/prompts/001-context-to-elisp-code.md")
;; (elmo-load-json-file "~/.emacs.d/lisp/elmo/workspace/resources/prompts/001-context-to-elisp-code.json")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))