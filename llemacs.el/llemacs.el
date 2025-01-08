;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 20:19:31
;;; Timestamp: <2025-01-08 20:19:31>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/llemacs.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--load-components ()
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (components '(
                       "01-llemacs-base"
                       "01-llemacs-path"
                       ;; "02-llemacs-secrets"
                       "02-llemacs-logging"
                       "03-llemacs-project"
                       "04-llemacs-llm"
                       "05-llemacs-run"
                       "07-llemacs-tools"
                       "08-llemacs-git"
                       "09-llemacs-integration"
                       )))
    (dolist (component components)
      (load (expand-file-name (concat component "/00-entry.el") dir)))))

(llemacs--load-components)

;; Initilize global logging files
;; (llemacs--logging-db-init-if)
;; (llemacs--logging-system-files-init-if)


(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
