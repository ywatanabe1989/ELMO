;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:23:59
;;; Time-stamp: <2024-12-31 23:23:59 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/05-llemacs-run/05-llemacs-run.el

(require '01-llemacs-config)
(require '02-llemacs-logging)
(require '02-llemacs-utils)
(require '04-llemacs-lang2elisp)

(defvar llemacs-tab-counter 0
  "Counter for LLEMACS tab numbering.")

(defun llemacs-before-run-hook (prompt)
  "Prepare environment before running LLEMACS operations."
  (condition-case err
      (llemacs-exec-local
       `(progn
          (llemacs--logging-note "
================================================================================
Llemacs-run called.
================================================================================")
          (llemacs-get-main-buffer)
          (setq llemacs-original-directory default-directory)
          (setq shell-file-name "/bin/bash")
          (setq python-shell-virtualenv-root "/workspace/.env")
          (setq python-shell-interpreter "/workspace/.env/bin/python3")))
    (error
     (llemacs--logging-error (format "Failed in before-run hook: %s" err))
     nil)))

(defun llemacs-run (prompt)
  "Main entry point for LLEMACS execution."
  (interactive)
  (let ((project-id (or (llemacs--get-current-project)
                        (llemacs-project-create))))
    (condition-case err
        (progn
          (llemacs--logging-init)
          (llemacs-project-ensure-dirs project-id)
          (let* ((context (llemacs-step-generate-context project-id))
                 (elisp-code (llemacs-step-generate-elisp context))
                 (result (llemacs-step-execute-elisp elisp-code project-id)))
            (llemacs-step-log-result context elisp-code result project-id)
            result))
      (error
       (llemacs--logging-error (format "Run failed: %s" err))
       nil))))

(defun llemacs-after-run-hook-success (elisp-code prompt-text)
  "Hook for successful LLEMACS operation."
  (llemacs--logging-prompt prompt-text)
  (llemacs--logging-success (format "Successfully executed:\n%s" elisp-code)))

(defun llemacs-after-run-hook-error (error prompt-text)
  "Hook for failed LLEMACS operation."
  (llemacs--logging-prompt prompt-text)
  (llemacs--logging-error (format "Llemacs-run failed.\n%s" error))
  (llemacs--logging-open))

(provide '06-llemacs-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))