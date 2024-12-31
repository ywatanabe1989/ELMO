;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:38:35
;;; Time-stamp: <2024-12-31 22:38:35 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/prompt/03-llemacs-llm-prompt-compiler.el

(defun llemacs--llm-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string
based on `llemacs--llm-prompt-recipes."
  (condition-case err
      (let* ((template (llemacs--llm-prompt-get-recipe recipe-id))
             (_ (unless template
                  (llemacs--logging-error (format "Template '%s' not found" recipe-id))
                  (error "Template '%s' not found" recipe-id)))
             (components (plist-get template :components))
             (content ""))
        (dolist (comp components)
          (let ((comp-file (expand-file-name
                            (concat comp ".md")
                            llemacs--llm-prompt-components-dir)))
            (unless (file-exists-p comp-file)
              (llemacs--logging-error (format "Component file not found: %s" comp-file))
              (error "Component file not found: %s" comp-file))
            (condition-case comp-err
                (setq content
                      (concat content ""
                              (with-temp-buffer
                                (insert (llemacs-load-markdown-file comp-file))
                                (insert "\n")
                                (buffer-string))))
              (error
               (llemacs--logging-error
                (format "Failed to load component '%s': %s"
                        comp (error-message-string comp-err)))
               (error "Failed to load component '%s'" comp)))))
        content)
    (error
     (llemacs--logging-error
      (format "Template compilation failed: %s"
              (error-message-string err)))
     nil)))

;; (llemacs--llm-prompt-compile "code-gen")



(defun llemacs--llm-prompt-combine (prompt template-name)
  "Embed PROMPT into template specified by TEMPLATE-NAME.
TEMPLATE-NAME must be a JSON file name without extension under `llemacs--path-prompt-compiled`."
  (condition-case err
      (let ((template (llemacs--llm-prompt-get-templates template-name)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (error
     (llemacs--logging-error (format "Failed to embed prompt:\n%s" err))
     nil)))

;; (llemacs--llm-prompt-get-templates)
;; (llemacs--llm-prompt-get-template "001-context-to-report")
;; (llemacs--llm-prompt-get-templates "001-context-to-report")
;; (llemacs--llm-prompt-combine "001-context-to-report" "hello")


(provide '03-llemacs-llm-prompt-main)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))