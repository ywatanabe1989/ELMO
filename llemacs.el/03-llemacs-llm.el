;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 22:40:19
;;; Time-stamp: <2024-12-31 22:40:19 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm.el

(defun llemacs--load-llm-components ()
  "Load LLM component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "03-llemacs-llm/core/03-llemacs-llm-api-configs.el" dir))
    (load (expand-file-name "03-llemacs-llm/core/03-llemacs-llm-api-keys-and-engines.el" dir))
    (load (expand-file-name "03-llemacs-llm/core/03-llemacs-llm-call.el" dir))
    (load (expand-file-name "03-llemacs-llm/core/03-llemacs-llm-call-providers.el" dir))
    (load (expand-file-name "03-llemacs-llm/core/03-llemacs-llm-helper-functions.el" dir))

    ;; Prompt components
    (load (expand-file-name "03-llemacs-llm/prompt/03-llemacs-llm-prompt-compiler.el" dir))
    (load (expand-file-name "03-llemacs-llm/prompt/03-llemacs-llm-prompt-recipe-helper-functions.el" dir))
    (load (expand-file-name "03-llemacs-llm/prompt/03-llemacs-llm-prompt-recipes.el" dir))))

;; Initialize LLM components
(llemacs--load-llm-components)

(provide '03-llemacs-llm)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))