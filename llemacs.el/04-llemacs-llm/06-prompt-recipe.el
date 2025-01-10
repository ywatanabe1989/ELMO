;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 04:43:59
;;; Timestamp: <2025-01-09 04:43:59>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/06-prompt-recipe.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'cl-lib)

(defvar llemacs--llm-prompt-recipes nil
  "List of prompt recipe definitions loaded from recipe files.")

(defun llemacs--load-recipe-file (file)
  "Load a recipe FILE from the recipes directory."
  (load (expand-file-name file) t t))

(defun llemacs--load-all-recipes ()
  "Load all recipe files and combine them."
  (let ((recipe-dir (concat llemacs--path-res-prompts "/recipes")))
    (dolist (file (directory-files recipe-dir t "\\.el$"))
      (llemacs--load-recipe-file file))
    ;; Return the loaded recipes
    (let ((recipes (mapcar #'symbol-value
                           (apropos-internal "^llemacs--recipe-.*"))))
      (setq llemacs--llm-prompt-recipes recipes)
      recipes)))

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
  (find-file llemacs--path-res-prompts))


(defun llemacs--llm-prompt-get-recipe (recipe-id)
  "Find and return a prompt recipe from `llemacs--llm-prompt-recipes` by its ID."
  (condition-case err
      (progn
        (unless llemacs--llm-prompt-recipes
          (llemacs--logging-write-error-pj "Template Recipes list is empty"))
        (let ((found (car (seq-filter (lambda (template)
                                        (equal recipe-id
                                               (plist-get template :id)))
                                      llemacs--llm-prompt-recipes))))
          (unless found
            (llemacs--logging-write-error-pj "Template Recipe not found: %s" recipe-id))
          found))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj "Failed to find template: %s" (error-message-string err)))))

(defun llemacs--llm-prompt-ensure-markdown-files ()
  "Convert JSON files to markdown if they are newer than their markdown counterparts."
  (dolist (json-file (directory-files llemacs--path-res-prompt-compiled t "\\.json$"))
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
          (llemacs--logging-write-error-pj "Template name required"))
        (unless (member prompt-template-name llemacs--llm-prompt-available-recipe-ids)
          (llemacs--logging-write-error-pj "Invalid prompt-template name: %s" prompt-template-name))
        (llemacs--load-markdown-file
         (expand-file-name (format "%s.md" prompt-template-name) llemacs--path-res-prompt-compiled)))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj "Error getting prompt-template: %s" (error-message-string err)))))


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
                (llemacs--logging-write-error-pj "Invalid prompt-template name: %s" name))
              (let ((content (llemacs--load-markdown-file
                              (expand-file-name (format "%s.md" name) llemacs--path-res-prompt-compiled))))
                (when content
                  (setq contents (concat contents "\n" content)))))
            (string-trim contents))))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj "Error getting prompt-templates: %s" (error-message-string err)))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
