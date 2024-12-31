;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:57:21
;;; Time-stamp: <2024-12-31 22:57:21 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/prompt/03-llemacs-llm-prompt-recipe-helper-functions.el

(defvar llemacs--llm-prompt-recipes
  '((:id "code-gen"
         :components ("roles/elisp-generator"
                      "tasks/code-generation"
                      "rules/code-elisp-format"
                      "tools/elisp"
                      "workspace/workspace"))
    (:id "report-gen"
         :components ("roles/report-generator"
                      "tasks/report-creation"
                      "rules/org-report-format"
                      "tools/python"
                      "workspace/workspace"))
    ))

(defun llemacs--llm-prompt-get-available-recipe-ids ()
  "Return a list of prompt recipe IDs from `llemacs--llm-prompt-recipes'."
  (mapcar (lambda (template)
            (plist-get template :id))
          llemacs--llm-prompt-recipes))

(defcustom llemacs--llm-prompt-available-recipe-ids
  (llemacs--llm-prompt-get-available-recipe-ids)
  "List of available prompt-recipe IDs."
  :type '(repeat string)
  :group 'llemacs)

(defun llemacs--llm-prompt-open-template-dir ()
  "Open the prompt template directory in Emacs using find-file."
  (interactive)
  (find-file llemacs--llm-prompt-template-dir))
;; (llemacs--llm-prompt-open-template-dir)

(defun llemacs--llm-prompt-get-recipe (recipe-id)
  "Find and return a prompt recipe from `llemacs--llm-prompt-recipes` by its ID."
  (condition-case err
      (progn
        (unless recipe-id
          (llemacs--logging-error "Template ID is required")
          (error "Template ID is required"))
        (unless llemacs--llm-prompt-recipes
          (llemacs--logging-error "Templates list is empty")
          (error "Templates list is empty"))
        (unless (member recipe-id llemacs--llm-prompt-available-recipe-ids)
          (error "Invalid template ID: %s. Available templates: %s"
                 recipe-id
                 (string-join llemacs--llm-prompt-available-recipe-ids ", ")))
        (let ((found (car (seq-filter (lambda (template)
                                        (string= recipe-id
                                                 (plist-get template :id)))
                                      llemacs--llm-prompt-recipes))))
          (unless found
            (error "Template not found: %s" recipe-id))
          found))
    (error
     (llemacs--logging-error (format "Failed to find template: %s"
                                     (error-message-string err)))
     nil)))
;; (llemacs--llm-prompt-get-recipe "code-gen")

(defun llemacs--llm-prompt-ensure-markdown-files ()
  "Convert JSON files to markdown if they are newer than their markdown counterparts."
  (dolist (json-file (directory-files llemacs--path-prompt-compiled t "\\.json$"))
    (unless (string-prefix-p "_" (file-name-nondirectory json-file))
      (let* ((md-file (concat (file-name-sans-extension json-file) ".md"))
             (json-time (file-attribute-modification-time (file-attributes json-file)))
             (md-exists (file-exists-p md-file))
             (md-time (and md-exists
                           (file-attribute-modification-time (file-attributes md-file)))))
        (when (or (not md-exists)
                  (time-less-p md-time json-time))
          (llemacs--cvt-json-to-markdown json-file))))))

(defun llemacs--llm-prompt-get-template (prompt-template-name)
  "Get content of PROMPT-TEMPLATE-NAME markdown file."
  (condition-case err
      (progn
        (llemacs--llm-prompt-ensure-markdown-files)
        (when (null prompt-template-name)
          (llemacs--logging-error "Template name required")
          (error "Template name required"))
        (unless (member prompt-template-name llemacs--llm-prompt-available-recipe-ids)
          (llemacs--logging-error (format "Invalid prompt-template name:\n%s" prompt-template-name))
          (error "Invalid prompt-template name"))
        (llemacs-load-markdown-file
         (expand-file-name (format "%s.md" prompt-template-name) llemacs--path-prompt-compiled)))
    (error
     (llemacs--logging-error (format "Error getting prompt-template:\n%s" err))
     nil)))

(defun llemacs--llm-prompt-get-templates (&rest prompt-template-names)
  "Get concatenated contents of PROMPT-TEMPLATE-NAMES markdown files.
If no PROMPT-TEMPLATE-NAMES provided, prompt-template user to select from available prompt-templates."
  (condition-case err
      (progn
        (llemacs--llm-prompt-ensure-markdown-files)
        (if (null prompt-template-names)
            (let ((selected (completing-read "Select prompt-template: " llemacs--llm-prompt-available-recipe-ids nil t)))
              (with-temp-buffer
                (insert (llemacs--llm-prompt-get-template selected))
                (buffer-string)))
          (let ((contents ""))
            (dolist (name prompt-template-names)
              (unless (member name llemacs--llm-prompt-available-recipe-ids)
                (llemacs--logging-error (format "Invalid prompt-template name:\n%s" name))
                (error "Invalid prompt-template name:\n%s" name))
              (let ((content (llemacs-load-markdown-file
                              (expand-file-name (format "%s.md" name) llemacs--path-prompt-compiled))))
                (when content
                  (setq contents (concat contents "\n" content)))))
            (string-trim contents))))
    (error
     (llemacs--logging-error (format "Error getting prompt-templates:\n%s" err))
     nil)))

(provide '03-llemacs--llm-prompt-recipe-helper-functions)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))