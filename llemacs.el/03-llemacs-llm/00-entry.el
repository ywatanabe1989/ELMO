;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:53:16
;;; Time-stamp: <2025-01-01 22:53:16 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/00_entry.el

(defun llemacs--load-llm-components ()
  "Load LLM component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "core-configs.el" dir))
    (load (expand-file-name "core-api-keys-and-engines.el" dir))
    (load (expand-file-name "core-call-wrapper.el" dir))
    (load (expand-file-name "core-call-providers.el" dir))
    (load (expand-file-name "core-helpers.el" dir))

    ;; Prompt components
    (load (expand-file-name "prompt-compile.el" dir))
    (load (expand-file-name "prompt-recipes.el" dir))))

;; Initialize LLM components
(llemacs--load-llm-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))