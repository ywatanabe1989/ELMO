;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 03:37:38
;;; Time-stamp: <2025-01-03 03:37:38 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-compile.el

(defun llemacs--llm-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string
based on `llemacs--llm-prompt-recipes`."
  (if (null recipe-id)
      "PLACEHOLDER"
    (condition-case err
        (let* ((template (llemacs--llm-prompt-get-recipe recipe-id))
               (_ (unless template
                    (llemacs--logging-log-error (format "Template Recipe '%s' not found" recipe-id))))
               (components (plist-get template :components))
               (content "PLACEHOLDER")
               (compiled-file (expand-file-name (concat recipe-id ".md") llemacs--path-prompt-compiled)))

          (unless (file-exists-p llemacs--path-prompt-compiled)
            (make-directory llemacs--path-prompt-compiled t))

          (dolist (component components)
            (let ((component-file (expand-file-name
                                   (concat component ".md")
                                   llemacs--path-prompt-components)))
              (unless (file-exists-p component-file)
                (llemacs--logging-log-error (format "Component file not found: %s" component-file)))
              (condition-case error
                  (setq content
                        (concat content
                                (with-temp-buffer
                                  (insert (llemacs--load-markdown-file component-file))
                                  (insert "\n")
                                  (buffer-string))))
                (error
                 (llemacs--logging-log-warn (format "Failed to load component '%s'" component))))))

          (with-temp-file compiled-file
            (insert content))

          content)
      (error
       (llemacs--logging-log-error
        (format "Template Recipe compilation failed: %s"
                (error-message-string err)))
       nil))))

;; (llemacs--llm-prompt-compile "code-gen")
;; (llemacs--llm-prompt-compile nil)

(defun llemacs--llm-prompt-embed (prompt recipe-id)
  "Embed PROMPT into template specified by RECIPE-ID."
  (condition-case err
      (let ((template (llemacs--llm-prompt-compile recipe-id)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (error
     (llemacs--logging-log-error (format "Failed to embed prompt:\n%s" err))
     nil)))

;; (llemacs--llm-prompt-embed "hello" nil)
;; (llemacs--llm-prompt-embed "hello" "code-gen")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))