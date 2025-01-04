;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 12:59:01
;;; Time-stamp: <2025-01-03 12:59:01 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-recipes/report-gen.el

;; recipes/report-gen.el
(defvar llemacs--recipe-report-gen
  '(:id "report-gen"
        :components ("roles/report-generator"
                     "tasks/code-generation"
                     "rules/code-format-elisp"
                     "rules/data-image-format"
                     "rules/data-movie-format"
                     "rules/results-org-report-format"
                     "rules/data-image-format"
                     "rules/data-saving"
                     "rules/proj-work-based-on-the-project-management"
                     "rules/proj-context-interpretation"
                     "rules/proj-update-context"
                     "example-io/elisp"
                     "workspace/workspace")))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))