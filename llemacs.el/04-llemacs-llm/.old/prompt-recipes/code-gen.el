;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 13:58:17
;;; Time-stamp: <2025-01-03 13:58:17 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/.old/prompt-recipes/code-gen.el

;; recipes/code-gen.el
(defvar llemacs--recipe-code-gen
  '(:id "code-gen"
        :components ("roles/elisp-generator"
                     "tasks/code-generation"
                     "rules/code-fix"
                     "rules/code-format-elisp"
                     "rules/code-format-shell"
                     "rules/code-logging"
                     "rules/code-refactor"
                     "rules/data-image-format"
                     "rules/data-movie-format"
                     "example-io/elisp"
                     "tools/elisp"
                     "workspace/workspace")))



;; main.el
(defvar llemacs--llm-prompt-recipes
  (list llemacs--recipe-code-gen
        llemacs--recipe-report-gen
        llemacs--recipe-nil))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))