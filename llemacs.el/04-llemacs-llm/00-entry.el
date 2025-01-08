;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 20:16:32
;;; Timestamp: <2025-01-08 20:16:32>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-llm-components ()
  "Load LLM component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Core components
    (load (expand-file-name "01-core-config.el" dir))
    (load (expand-file-name "02-core-api-key-and-engine.el" dir))
    (load (expand-file-name "03-core-call-provider.el" dir))
    (load (expand-file-name "04-core-call-wrapper.el" dir))
    (load (expand-file-name "05-core-helper.el" dir))

    ;; Prompt components
    (load (expand-file-name "06-prompt-recipe.el" dir))
    (load (expand-file-name "07-prompt-compile.el" dir))
    (load (expand-file-name "08-prompt-to-elisp.el" dir))
    (load (expand-file-name "09-prompt-to-progn.el" dir))
    ))

;; Initialize LLM components
(llemacs--load-llm-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
