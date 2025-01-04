;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 12:59:43
;;; Time-stamp: <2025-01-03 12:59:43 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-recipes/main.el

;; main.el
(defvar llemacs--llm-prompt-recipes
  (list llemacs--recipe-code-gen
        llemacs--recipe-report-gen
        llemacs--recipe-nil))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))