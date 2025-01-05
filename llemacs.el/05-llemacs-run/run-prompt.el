;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 04:04:13
;;; Time-stamp: <2025-01-05 04:04:13 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-prompt.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 23:21:47
;;; Time-stamp: <2025-01-04 23:21:47 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-prompt.el

(defun llemacs--run-prompt (prompt &optional recipe-id)
  "Main entry point for LLEMACS execution."
  (interactive "sPrompt: ")
  (condition-case err
      (progn
        (let* ((default-recipe "code-gen")
               (recipe-id (or recipe-id default-recipe))
               (full-prompt (llemacs--llm-prompt-embed prompt recipe-id)))
          (llemacs--logging-write-prompt-pj full-prompt)
          (let ((elisp-code (llemacs--cvt-prompt2elisp full-prompt)))
            (llemacs--logging-write-elisp-pj elisp-code)
            (llemacs--run-elisp elisp-code))))
    (error
     (llemacs--logging-write-error-pj (format "Run failed: %s" err))
     nil)))

(defun llemacs--run-before-run-hook (prompt)
  "Prepare environment before running LLEMACS operations."
  (condition-case err
      (llemacs-exec-local
       `(progn
          (llemacs--logging-note llemacs--run-start-tag)
          (llemacs-get-main-buffer)
          (setq llemacs-original-directory default-directory)
          (setq shell-file-name "/bin/bash")
          (setq python-shell-virtualenv-root "/workspace/.env")
          (setq python-shell-interpreter "/workspace/.env/bin/python3")))
    (error
     (llemacs--logging-write-error-pj (format "Failed in before-run hook: %s" err))
     nil)))

(defun llemacs--run-prompt-after-run-hook-success (elisp-code prompt-text)
  "Hook for successful LLEMACS operation."
  (llemacs--logging-write-prompt-pj prompt-text)
  (llemacs--logging-write-success-pj (format "Successfully executed:\n%s" elisp-code)))

(defun llemacs--run-prompt-after-run-hook-error (error prompt-text)
  "Hook for failed LLEMACS operation."
  (llemacs--logging-write-prompt-pj prompt-text)
  (llemacs--logging-write-error-pj (format "llemacs-run-prompt failed.\n%s" error))
  (llemacs--logging-open))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))