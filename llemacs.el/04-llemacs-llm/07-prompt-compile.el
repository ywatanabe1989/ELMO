;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 22:05:38
;;; Timestamp: <2025-01-10 22:05:38>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/07-prompt-compile.el

(defun llemacs--llm-prompt-embed (prompt recipe-id)
  "Embed PROMPT into template specified by RECIPE-ID."
  (condition-case err
      (let ((template (llemacs--llm-prompt-compile recipe-id)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (llemacs--logging-write-error-pj
     (llemacs--logging-write-error-pj (format "Failed to embed prompt:\n%s" err))
     nil)))

(defun llemacs--llm-prompt-write-compiled (recipe-id content)
  "Write compiled CONTENT to file for RECIPE-ID."
  (let ((output-file (expand-file-name
                      (concat recipe-id ".md")
                      llemacs--path-res-prompt-compiled)))
    (with-temp-file output-file
      (insert content))))

;; (defun llemacs--llm-prompt-compile (recipe-id)
;;   "Compile a prompt template into a single content string based on `llemacs--llm-prompt-recipes`."
;;   (llemacs--logging-write-debug-pj
;;    (format "Starting prompt compilation for recipe: %s" recipe-id))
;;   (let* ((recipe-path (expand-file-name (concat recipe-id ".yaml") llemacs--path-res-prompt-recipes))
;;          (splitter-1 "====================\n")
;;          (splitter-2 "--------------------\n")
;;          (start-header (format "%sRecipe Starts (ID: %s)\n%s" splitter-1 recipe-id splitter-1))
;;          (end-header (format "%sRecipe Ends (ID: %s)\n%s" splitter-1 recipe-id splitter-1))
;;          (content "")
;;          )
;;     (if (not (file-exists-p recipe-path))
;;         (progn
;;           (llemacs--logging-write-error-pj
;;            (format "Recipe file not found: %s" recipe-path))
;;           nil)
;;       (condition-case err
;;           (let* ((yaml-data (llemacs--load-yaml-file recipe-path))
;;                  (roles (or (alist-get 'roles yaml-data) '()))
;;                  (tasks (or (alist-get 'tasks yaml-data) '()))
;;                  (rules (or (alist-get 'rules yaml-data) '()))
;;                  (examples (or (alist-get 'examples yaml-data) '()))
;;                  (resources (or (alist-get 'resources yaml-data) '()))
;;                  (tools (or (alist-get 'tools yaml-data) '()))
;;                  (requests (or (alist-get 'requests yaml-data) '()))
;;                  )
;;             (dolist (section-pair
;;                      `((roles . ,roles)
;;                        (tasks . ,tasks)
;;                        (rules . ,rules)
;;                        (examples . ,examples)
;;                        (resources . ,resources)
;;                        (tools . ,tools)
;;                        (requests . ,requests)))
;;               (let ((section-type (symbol-name (car section-pair)))
;;                     (components (cdr section-pair)))
;;                 (when components
;;                   (setq content (concat content
;;                                         ;; (format "%s (Please ignore when not applicable for the current request)\n%s" (upcase section-type) splitter-1))))
;;                                         (format "%s\n%s" (upcase section-type) splitter-1))))
;;                 (dolist (component components)
;;                   (let ((component-file (expand-file-name
;;                                          (concat component ".md")
;;                                          (expand-file-name section-type llemacs--path-res-prompt-components))))
;;                     (if (not (file-exists-p component-file))
;;                         (llemacs--logging-write-error-pj
;;                          (format "Component file not found: %s" component-file))
;;                       (setq content
;;                             (concat content
;;                                     (string-trim
;;                                      (llemacs--load-markdown-file component-file))
;;                                     (format "\n%s" splitter-2))))))

;;                 (when components
;;                   (setq content (concat content splitter-1)))))
;;             (llemacs--llm-prompt-write-compiled recipe-id content)
;;             (concat start-header content end-header))
;;         (llemacs--logging-write-error-pj
;;          (llemacs--logging-write-error-pj
;;           (format "Template Recipe compilation failed:\n%s" err))
;;          nil)))))

(defun llemacs--llm-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string based on `llemacs--llm-prompt-recipes`."
  (llemacs--logging-write-debug-pj
   (format "Starting prompt compilation for recipe: %s" recipe-id))
  (let* ((recipe-path (expand-file-name (concat recipe-id ".yaml") llemacs--path-res-prompt-recipes))
         (splitter-1 "====================\n")
         (splitter-2 "--------------------\n")
         (start-header (format "%sRecipe Starts (ID: %s)\n%s" splitter-1 recipe-id splitter-1))
         (end-header (format "%sRecipe Ends (ID: %s)\n%s" splitter-1 recipe-id splitter-1))
         (content ""))

    (unless (file-exists-p recipe-path)
      (llemacs--logging-write-error-pj "Recipe file not found: %s" recipe-path))

    (condition-case err
        (let* ((yaml-data (llemacs--load-yaml-file recipe-path))
               (roles (or (alist-get 'roles yaml-data) '()))
               (tasks (or (alist-get 'tasks yaml-data) '()))
               (rules (or (alist-get 'rules yaml-data) '()))
               (examples (or (alist-get 'examples yaml-data) '()))
               (resources (or (alist-get 'resources yaml-data) '()))
               (tools (or (alist-get 'tools yaml-data) '()))
               (requests (or (alist-get 'requests yaml-data) '())))

          (dolist (section-pair
                   `((roles . ,roles)
                     (tasks . ,tasks)
                     (rules . ,rules)
                     (examples . ,examples)
                     (resources . ,resources)
                     (tools . ,tools)
                     (requests . ,requests)))
            (let ((section-type (symbol-name (car section-pair)))
                  (components (cdr section-pair)))
              (when components
                (setq content (concat content
                                      (format "%s\n%s" (upcase section-type) splitter-1))))
              (dolist (component components)
                (let ((component-file (expand-file-name
                                       (concat component ".md")
                                       (expand-file-name section-type llemacs--path-res-prompt-components))))
                  (unless (file-exists-p component-file)
                    (llemacs--logging-write-error-pj "Component file not found: %s" component-file))
                  (setq content
                        (concat content
                                (string-trim
                                 (llemacs--load-markdown-file component-file))
                                (format "\n%s" splitter-2)))))
              (when components
                (setq content (concat content splitter-1)))))

          (llemacs--llm-prompt-write-compiled recipe-id content)
          (concat start-header content end-header))
      (llemacs--logging-write-error-pj
       (llemacs--logging-write-error-pj "Template Recipe compilation failed: %s" (error-message-string err))))))

;; (llemacs--llm-prompt-compile "code-elisp-progn")
;; (llemacs--llm-prompt-compile "project-management")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
