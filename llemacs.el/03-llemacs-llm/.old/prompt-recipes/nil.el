;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 12:59:10
;;; Time-stamp: <2025-01-03 12:59:10 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/prompt-recipes/nil.el

;; recipes/nil.el
(defvar llemacs--recipe-nil
  '(:id nil
        :components ("roles/nil")))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))