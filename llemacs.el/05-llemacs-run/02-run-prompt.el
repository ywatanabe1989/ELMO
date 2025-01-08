;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 08:30:39
;;; Timestamp: <2025-01-09 08:30:39>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/02-run-prompt.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ;; working but only for the first three figures
;; (defun llemacs--run-prompt (prompt &optional recipe-id)
;;   "Main entry point for LLEMACS execution."
;;   (interactive "sPrompt: ")
;;   (let ((elisp-code nil))
;;     (condition-case err
;;         (let* ((default-recipe "code-elisp-progn")
;;                (recipe-id (or recipe-id default-recipe))
;;                (full-prompt (llemacs--llm-prompt-embed prompt recipe-id)))
;;           (llemacs--logging-write-prompt-pj full-prompt)
;;           (llemacs--timestamp-set)
;;           (setq elisp-code (llemacs--llm-prompt2elisp full-prompt))
;;           (llemacs--run-elisp elisp-code))  ; Remove read here
;;       (error
;;        (error "Run failed: %s\nCode: %S" (error-message-string err) elisp-code)))))

;; ;; org is produced but pdf is not
;; (defun llemacs--run-prompt (prompt &optional recipe-id)
;;   "Main entry point for LLEMACS execution."
;;   (interactive "sPrompt: ")
;;   (let ((elisp-code nil))
;;     (condition-case err
;;         (let* ((default-recipe "code-elisp-progn")
;;                (recipe-id (or recipe-id default-recipe))
;;                ;; (full-prompt (llemacs--llm-prompt-embed prompt recipe-id))
;;                )
;;           ;; (llemacs--logging-write-prompt-pj prompt)
;;           (llemacs--timestamp-set)
;;           (setq elisp-code (llemacs--llm-prompt2elisp prompt recipe-id))
;;           (if (stringp elisp-code)
;;               (llemacs--run-elisp (read elisp-code))
;;             (llemacs--run-elisp elisp-code)))
;;       (error
;;        (error "Run failed: %s\nCode: %S" (error-message-string err) elisp-code)))))

;; (llemacs--run-prompt "plot something" "code-elisp-progn")

(defun llemacs--run-prompt (prompt &optional recipe-id)
  "Main entry point for LLEMACS execution."
  (interactive "sPrompt: ")
  (let ((elisp-code nil))
    (condition-case err
        (let* ((default-recipe "code-elisp-progn")
               (recipe-id (or recipe-id default-recipe)))
          (llemacs--timestamp-set)
          (setq elisp-code (llemacs--llm-prompt2elisp prompt recipe-id))
          (llemacs--run-elisp elisp-code))
      (error
       (error "Run failed: %s\nCode: %S" (error-message-string err) elisp-code)))))



(defun llemacs--run-prompt-after-run-hook-success (elisp-code prompt-text)
  "Hook for successful LLEMACS operation."
  (llemacs--logging-write-prompt-pj prompt-text)
  (llemacs--logging-write-success-pj (format "Successfully executed:\n%s" elisp-code)))

(defun llemacs--run-prompt-after-run-hook-error (error prompt-text)
  "Hook for failed LLEMACS operation."
  (llemacs--logging-write-prompt-pj prompt-text)
  (llemacs--logging-open)
  (error "llemacs-run-prompt failed.\n%s" error))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
