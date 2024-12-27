;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 18:36:03
;;; Time-stamp: <2024-12-27 18:36:03 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/08-elmo-prompt-templates.el

(require '01-elmo-config)
(require '02-elmo-logging-core)
;; (require '03-elmo-logging-utils)

(defcustom elmo-prompt-templates-dir
  (expand-file-name "resources/prompt-templates" elmo-workspace-dir)
  "Directory for prompt templates."
  :type 'file
  :group 'elmo)
;; ~/.emacs.d/lisp/elmo/workspace/resources/prompt-templates/001-context-to-elisp-code.json

(defun elmo-get-available-prompt-templates ()
  "Get list of available prompt-template by scanning prompt-template directory."
  (let ((files (directory-files elmo-prompt-templates-dir nil "\\.json$")))
    (mapcar (lambda (f)
              (string-remove-suffix ".json" f))
            (cl-remove-if (lambda (f)
                            (string-prefix-p "_" f))
                          files))))

(defcustom elmo-available-prompt-templates
  (elmo-get-available-prompt-templates)
  "List of available prompt-template names."
  :type '(repeat string)
  :group 'elmo)

(defun elmo-prompt-templates-ensure-markdown-files ()
  "Convert JSON files to markdown if they are newer than their markdown counterparts."
  (dolist (json-file (directory-files elmo-prompt-templates-dir t "\\.json$"))
    (unless (string-prefix-p "_" (file-name-nondirectory json-file))
      (let* ((md-file (concat (file-name-sans-extension json-file) ".md"))
             (json-time (file-attribute-modification-time (file-attributes json-file)))
             (md-exists (file-exists-p md-file))
             (md-time (and md-exists
                           (file-attribute-modification-time (file-attributes md-file)))))
        (when (or (not md-exists)
                  (time-less-p md-time json-time))
          (elmo-json-to-markdown json-file))))))

(defun elmo-get-prompt-template (prompt-template-name)
  "Get content of PROMPT-TEMPLATE-NAME markdown file."
  (condition-case err
      (progn
        (elmo-prompt-templates-ensure-markdown-files)
        (when (null prompt-template-name)
          (elmo-log-error "Template name required")
          (error "Template name required"))
        (unless (member prompt-template-name elmo-available-prompt-templates)
          (elmo-log-error (format "Invalid prompt-template name:\n%s" prompt-template-name))
          (error "Invalid prompt-template name"))
        (elmo-load-markdown-file
         (expand-file-name (format "%s.md" prompt-template-name) elmo-prompt-templates-dir)))
    (error
     (elmo-log-error (format "Error getting prompt-template:\n%s" err))
     nil)))

(defun elmo-get-prompt-templates (&rest prompt-template-names)
  "Get concatenated contents of PROMPT-TEMPLATE-NAMES markdown files.
If no PROMPT-TEMPLATE-NAMES provided, prompt-template user to select from available prompt-templates."
  (condition-case err
      (progn
        (elmo-prompt-templates-ensure-markdown-files)
        (if (null prompt-template-names)
            (let ((selected (completing-read "Select prompt-template: " elmo-available-prompt-templates nil t)))
              (with-temp-buffer
                (insert (elmo-get-prompt-template selected))
                (buffer-string)))
          (let ((contents ""))
            (dolist (name prompt-template-names)
              (unless (member name elmo-available-prompt-templates)
                (elmo-log-error (format "Invalid prompt-template name:\n%s" name))
                (error "Invalid prompt-template name:\n%s" name))
              (let ((content (elmo-load-markdown-file
                              (expand-file-name (format "%s.md" name) elmo-prompt-templates-dir))))
                (when content
                  (setq contents (concat contents "\n" content)))))
            (string-trim contents))))
    (error
     (elmo-log-error (format "Error getting prompt-templates:\n%s" err))
     nil)))

(defun elmo-to-full-prompt (template-name prompt-text)
  "Embed PROMPT-TEXT into template specified by TEMPLATE-NAME."
  (condition-case err
      (let ((template (elmo-get-prompt-templates template-name)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt-text template t t)))
    (error
     (elmo-log-error (format "Failed to embed prompt:\n%s" err))
     nil)))

;; (elmo-get-prompt-templates)
;; (elmo-get-prompt-template "001-context-to-elisp-code")
;; (elmo-get-prompt-templates "001-context-to-elisp-code")
;; (elmo-to-full-prompt "001-context-to-elisp-code" "hello")


(provide '08-elmo-prompt-templates)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))