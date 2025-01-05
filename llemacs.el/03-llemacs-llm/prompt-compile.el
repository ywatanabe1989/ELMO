;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 16:16:32
;;; Time-stamp: <2025-01-05 16:16:32 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-compile.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 01:09:08
;;; Time-stamp: <2025-01-05 01:09:08 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-compile.el

(defun llemacs--llm-prompt-compile (recipe-id)
  "Compile a prompt template into a single content string
based on `llemacs--llm-prompt-recipes`."
  (llemacs--load-all-recipes)
  (condition-case err
      (let* ((template (llemacs--llm-prompt-get-recipe recipe-id))
             (components (and template (plist-get template :components)))
             (content ""))
        (if (not template)
            (progn
              (llemacs--logging-write-error-pj
               (format "Template Recipe '%s' not found" recipe-id))
              nil)
          (dolist (component components)
            (let ((component-file (expand-file-name
                                   (concat component ".md")
                                   llemacs--path-res-prompt-components)))
              (if (not (file-exists-p component-file))
                  (progn
                    (llemacs--logging-write-error-pj
                     (format "Component file not found: %s" component-file))
                    nil)
                (setq content
                      (concat content
                              (string-trim
                               (llemacs--load-markdown-file component-file))
                              "\n\n")))))
          content))
    (error
     (llemacs--logging-write-error-pj (format "Template Recipi compilation failed:\n%s" err))
     ;; (llemacs--logging-write-error-pj
     ;;  (format-message "Template Recipe compilation failed: %s"
     ;;                  (error-message-string err)))
     nil)))

(defun llemacs--llm-prompt-embed (prompt recipe-id)
  "Embed PROMPT into template specified by RECIPE-ID."
  (condition-case err
      (let ((template (llemacs--llm-prompt-compile recipe-id)))
        (when template
          (replace-regexp-in-string "PLACEHOLDER" prompt template t t)))
    (error
     (llemacs--logging-write-error-pj (format "Failed to embed prompt:\n%s" err))
     nil)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))