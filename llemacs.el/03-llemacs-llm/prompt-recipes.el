;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 03:04:58
;;; Time-stamp: <2025-01-03 03:04:58 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-recipes.el

(defvar llemacs--llm-prompt-recipes
  '(
    (:id "code-gen"
         :components ("roles/elisp-generator"
                      "tasks/code-generation"
                      "rules/code-format-elisp"
                      "rules/code-logging"
                      "rules/code-fix"
                      "example-io/elisp"
                      "tools/elisp"
                      "workspace/workspace"))
    (:id "report-gen"
         :components ("roles/report-generator"
                      "tasks/report-creation"
                      "rules/code-format-elisp"
                      "rules/org-report-format"
                      "rules/data-image-format"
                      "rules/data-saving"
                      "rules/code-logging"
                      "rules/code-fix"
                      "example-io/elisp"
                      "tools/python"
                      "workspace/workspace"))
    (:id nil
         :components ("roles/nil"))
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

(defun llemacs--llm-prompt-open-templates ()
  "Open the prompt template directory in Emacs using find-file."
  (interactive)
  (find-file llemacs--path-prompt-templates))
;; (llemacs--llm-prompt-open-templates)

(defun llemacs--llm-prompt-get-recipe (recipe-id)
  "Find and return a prompt recipe from `llemacs--llm-prompt-recipes` by its ID."
  (condition-case err
      (let ((err-msg nil))
        (unless llemacs--llm-prompt-recipes
          (setq err-msg "Template Recipes list is empty")
          (llemacs--logging-log-error err-msg)
          (error err-msg))
        (let ((found (car (seq-filter (lambda (template)
                                        (equal recipe-id
                                               (plist-get template :id)))
                                      llemacs--llm-prompt-recipes))))
          (unless found
            (setq err-msg (format "Template Recipe not found: %s" recipe-id))
            (llemacs--logging-log-error err-msg)
            (error err-msg))
          found))
    (error
     (llemacs--logging-log-error (format "Failed to find template: %s"
                                         (error-message-string err)))
     nil)))

;; (llemacs--llm-prompt-get-recipe "code-gen")
;; (llemacs--llm-prompt-get-recipe nil)

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
          (llemacs--logging-log-error "Template name required")
          (error "Template name required"))
        (unless (member prompt-template-name llemacs--llm-prompt-available-recipe-ids)
          (llemacs--logging-log-error (format "Invalid prompt-template name:\n%s" prompt-template-name))
          (error "Invalid prompt-template name"))
        (llemacs--load-markdown-file
         (expand-file-name (format "%s.md" prompt-template-name) llemacs--path-prompt-compiled)))
    (error
     (llemacs--logging-log-error (format "Error getting prompt-template:\n%s" err))
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
                (llemacs--logging-log-error (format "Invalid prompt-template name:\n%s" name))
                (error "Invalid prompt-template name:\n%s" name))
              (let ((content (llemacs--load-markdown-file
                              (expand-file-name (format "%s.md" name) llemacs--path-prompt-compiled))))
                (when content
                  (setq contents (concat contents "\n" content)))))
            (string-trim contents))))
    (error
     (llemacs--logging-log-error (format "Error getting prompt-templates:\n%s" err))
     nil)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))