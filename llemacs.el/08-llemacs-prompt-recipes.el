;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 17:01:54
;;; Time-stamp: <2024-12-29 17:01:54 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/08-llemacs-prompt-recipes.el

(defvar llemacs-prompt-recipes
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

(provide '08-llemacs-prompt-recipes)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))