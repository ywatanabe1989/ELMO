;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 05:26:15
;;; Timestamp: <2025-01-08 05:26:15>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 22:53:16
;;; Time-stamp: <2025-01-01 22:53:16 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/00_entry.el

(defun llemacs--load-llm-components ()
  "Load LLM component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "01-core-configs.el" dir))
    (load (expand-file-name "02-core-api-keys-and-engines.el" dir))
    (load (expand-file-name "03-core-call-providers.el" dir))
    (load (expand-file-name "04-core-call-wrapper.el" dir))
    (load (expand-file-name "05-core-helpers.el" dir))

    ;; Prompt components
    (load (expand-file-name "06-prompt-compile.el" dir))
    (load (expand-file-name "07-prompt-recipes.el" dir))
    (load (expand-file-name "08-prompt2elisp.el" dir))))

;; Initialize LLM components
(llemacs--load-llm-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
