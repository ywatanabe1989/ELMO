;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 15:53:20
;;; Time-stamp: <2025-01-04 15:53:20 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-compile.el

(defun llemacs--llm-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string
based on `llemacs--llm-prompt-recipes`."
  (llemacs--load-all-recipes)
  (condition-case err
      (let* ((template (llemacs--llm-prompt-get-recipe recipe-id))
             (_ (unless template
                  (llemacs--logging-write-error-pj (format "Template Recipe '%s' not found" recipe-id))))
             (components (plist-get template :components))
             (content "")
             (compiled-file (expand-file-name (concat (or recipe-id "nil") ".md")
                                              llemacs--path-res-prompt-compiled)))
        (unless (file-exists-p llemacs--path-res-prompt-compiled)
          (make-directory llemacs--path-res-prompt-compiled t))
        (dolist (component components)
          (let ((component-file (expand-file-name
                                 (concat component ".md")
                                 llemacs--path-res-prompt-components)))
            (unless (file-exists-p component-file)
              (llemacs--logging-write-error-pj (format "Component file not found: %s" component-file)))
            (condition-case error
                (setq content
                      (concat content
                              (string-trim
                               (llemacs--load-markdown-file component-file))
                              "\n\n"))
              (error
               (llemacs--logging-write-warn-pj (format "Failed to load component '%s'" component))))))
        (llemacs--logging-write-prompt-pj content)
        content)
    (error
     (llemacs--logging-write-error-pj
      (format "Template Recipe compilation failed: %s"
              (error-message-string err)))
     nil)))
;; (llemacs--buffer-display-prompt llemacs--path-project-prompt)
;; (llemacs--llm-prompt-compile "code-gen")
;; (llemacs--llm-prompt-compile nil)

(defun llemacs--llm-prompt-embed (prompt recipe-id)
  "Embed PROMPT into template specified by RECIPE-ID."
  (condition-case err
      (let ((template (llemacs--llm-prompt-compile recipe-id)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (error
     (llemacs--logging-write-error-pj (format "Failed to embed prompt:\n%s" err))
     nil)))

;; (llemacs--llm-prompt-embed "hello" nil)
;; (llemacs--llm-prompt-embed "hello" "code-gen")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))