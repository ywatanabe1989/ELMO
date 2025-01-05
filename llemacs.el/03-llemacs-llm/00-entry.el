;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 16:24:29
;;; Time-stamp: <2025-01-05 16:24:29 (ywatanabe)>
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