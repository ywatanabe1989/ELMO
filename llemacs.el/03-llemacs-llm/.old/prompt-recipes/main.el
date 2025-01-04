;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
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