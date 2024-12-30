;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 17:02:06
;;; Time-stamp: <2024-12-29 17:02:06 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/08-llemacs-prompt.el

(require '01-llemacs-config)
(require '02-llemacs-logging-core)
(require '06-llemacs-json-md)
(require '08-llemacs-prompt-recipes)

(defcustom llemacs-prompt-template-dir
  (expand-file-name "resources/prompt-templates" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs-prompt-compiled-dir
  (expand-file-name "resources/prompt-templates/compiled" llemacs-path-workspace)
  "Directory for prompt templates."
  :type 'file
  :group 'llemacs)

(defcustom llemacs-prompt-components-dir
  (expand-file-name "resources/prompt-templates/components" llemacs-path-workspace)
  "Directory for prompt components."
  :type 'file
  :group 'llemacs)

(defun llemacs-prompt-get-available-recipe-ids ()
  "Return a list of prompt recipe IDs from `llemacs-prompt-recipes'."
  (mapcar (lambda (template)
            (plist-get template :id))
          llemacs-prompt-recipes))

(defcustom llemacs-prompt-available-recipe-ids
  (llemacs-prompt-get-available-recipe-ids)
  "List of available prompt-recipe IDs."
  :type '(repeat string)
  :group 'llemacs)

(defun llemacs-prompt-open-template-dir ()
  "Open the prompt template directory in Emacs using find-file."
  (interactive)
  (find-file llemacs-prompt-template-dir))
;; (llemacs-prompt-open-template-dir)

(defun llemacs-prompt-get-recipe (recipe-id)
  "Find and return a prompt recipe from `llemacs-prompt-recipes` by its ID."
  (condition-case err
      (progn
        (unless recipe-id
          (llemacs--log-error "Template ID is required")
          (error "Template ID is required"))
        (unless llemacs-prompt-recipes
          (llemacs--log-error "Templates list is empty")
          (error "Templates list is empty"))
        (unless (member recipe-id llemacs-prompt-available-recipe-ids)
          (error "Invalid template ID: %s. Available templates: %s"
                 recipe-id
                 (string-join llemacs-prompt-available-recipe-ids ", ")))
        (let ((found (car (seq-filter (lambda (template)
                                        (string= recipe-id
                                                 (plist-get template :id)))
                                      llemacs-prompt-recipes))))
          (unless found
            (error "Template not found: %s" recipe-id))
          found))
    (error
     (llemacs--log-error (format "Failed to find template: %s"
                             (error-message-string err)))
     nil)))
;; (llemacs-prompt-get-recipe "code-gen")

(defun llemacs-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string
based on `llemacs-prompt-recipes."
  (condition-case err
      (let* ((template (llemacs-prompt-get-recipe recipe-id))
             (_ (unless template
                  (llemacs--log-error (format "Template '%s' not found" recipe-id))
                  (error "Template '%s' not found" recipe-id)))
             (components (plist-get template :components))
             (content ""))
        (dolist (comp components)
          (let ((comp-file (expand-file-name
                            (concat comp ".md")
                            llemacs-prompt-components-dir)))
            (unless (file-exists-p comp-file)
              (llemacs--log-error (format "Component file not found: %s" comp-file))
              (error "Component file not found: %s" comp-file))
            (condition-case comp-err
                (setq content
                      (concat content ""
                              (with-temp-buffer
                                (insert (llemacs-load-markdown-file comp-file))
                                (insert "\n")
                                (buffer-string))))
              (error
               (llemacs--log-error
                (format "Failed to load component '%s': %s"
                        comp (error-message-string comp-err)))
               (error "Failed to load component '%s'" comp)))))
        content)
    (error
     (llemacs--log-error
      (format "Template compilation failed: %s"
              (error-message-string err)))
     nil)))

;; (llemacs-prompt-compile "code-gen")

(defun llemacs-prompt-ensure-markdown-files ()
  "Convert JSON files to markdown if they are newer than their markdown counterparts."
  (dolist (json-file (directory-files llemacs-prompt-compiled-dir t "\\.json$"))
    (unless (string-prefix-p "_" (file-name-nondirectory json-file))
      (let* ((md-file (concat (file-name-sans-extension json-file) ".md"))
             (json-time (file-attribute-modification-time (file-attributes json-file)))
             (md-exists (file-exists-p md-file))
             (md-time (and md-exists
                           (file-attribute-modification-time (file-attributes md-file)))))
        (when (or (not md-exists)
                  (time-less-p md-time json-time))
          (llemacs-json-to-markdown json-file))))))

(defun llemacs-prompt-get-template (prompt-template-name)
  "Get content of PROMPT-TEMPLATE-NAME markdown file."
  (condition-case err
      (progn
        (llemacs-prompt-ensure-markdown-files)
        (when (null prompt-template-name)
          (llemacs--log-error "Template name required")
          (error "Template name required"))
        (unless (member prompt-template-name llemacs-prompt-available-recipe-ids)
          (llemacs--log-error (format "Invalid prompt-template name:\n%s" prompt-template-name))
          (error "Invalid prompt-template name"))
        (llemacs-load-markdown-file
         (expand-file-name (format "%s.md" prompt-template-name) llemacs-prompt-compiled-dir)))
    (error
     (llemacs--log-error (format "Error getting prompt-template:\n%s" err))
     nil)))

(defun llemacs-prompt-get-templates (&rest prompt-template-names)
  "Get concatenated contents of PROMPT-TEMPLATE-NAMES markdown files.
If no PROMPT-TEMPLATE-NAMES provided, prompt-template user to select from available prompt-templates."
  (condition-case err
      (progn
        (llemacs-prompt-ensure-markdown-files)
        (if (null prompt-template-names)
            (let ((selected (completing-read "Select prompt-template: " llemacs-prompt-available-recipe-ids nil t)))
              (with-temp-buffer
                (insert (llemacs-prompt-get-template selected))
                (buffer-string)))
          (let ((contents ""))
            (dolist (name prompt-template-names)
              (unless (member name llemacs-prompt-available-recipe-ids)
                (llemacs--log-error (format "Invalid prompt-template name:\n%s" name))
                (error "Invalid prompt-template name:\n%s" name))
              (let ((content (llemacs-load-markdown-file
                              (expand-file-name (format "%s.md" name) llemacs-prompt-compiled-dir))))
                (when content
                  (setq contents (concat contents "\n" content)))))
            (string-trim contents))))
    (error
     (llemacs--log-error (format "Error getting prompt-templates:\n%s" err))
     nil)))

(defun llemacs-prompt-combine (prompt template-name)
  "Embed PROMPT into template specified by TEMPLATE-NAME.
TEMPLATE-NAME must be a JSON file name without extension under `llemacs-prompt-compiled-dir`."
  (condition-case err
      (let ((template (llemacs-prompt-get-templates template-name)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (error
     (llemacs--log-error (format "Failed to embed prompt:\n%s" err))
     nil)))

;; (llemacs-prompt-get-templates)
;; (llemacs-prompt-get-template "001-context-to-report")
;; (llemacs-prompt-get-templates "001-context-to-report")
;; (llemacs-prompt-combine "001-context-to-report" "hello")


(provide '08-llemacs-prompt)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))