;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:11:26
;;; Time-stamp: <2024-12-29 08:11:26 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/10-llemacs-run.el

(require '01-llemacs-config)
(require '02-llemacs-logging-core)
(require '04-llemacs-utils)
(require '09-llemacs-lang2elisp)

(defvar llemacs-tab-counter 0
  "Counter for ELMO tab numbering.")

(defvar llemacs-tab-counter 0
  "Counter for ELMO tab numbering.

Example:
  (setq llemacs-tab-counter 0)
  (1+ llemacs-tab-counter) ;; => 1")

(defun llemacs-before-run-hook (prompt)
  "Prepare environment before running ELMO operations."
  (condition-case err
      (llemacs-exec-local
       `(progn
          (llemacs--log-note "
================================================================================
Elmo-run called.
================================================================================")
          (llemacs-get-main-buffer)
          (setq llemacs-original-directory default-directory)
          (setq shell-file-name "/bin/bash")
          (setq python-shell-virtualenv-root "/workspace/.env")
          (setq python-shell-interpreter "/workspace/.env/bin/python3")))
    (error
     (llemacs--log-error (format "Failed in before-run hook: %s" err))
     nil)))


(defun llemacs-run (&optional prompt)
  "Run ELMO operations with given PROMPT."
  (interactive)
  (let ((prompt-text (or prompt (read-string "ELMO prompt: ") ""))
        (original-dir default-directory))
    (llemacs-before-run-hook prompt-text)
    (condition-case err
        (let ((elisp-code (llemacs-lang2elisp prompt-text)))
          (unless elisp-code
            (signal 'error '("No valid elisp code generated")))
          (llemacs-get-main-buffer)
          (llemacs-exec-local elisp-code)
          (cd original-dir)
          (llemacs-after-run-hook-success elisp-code prompt-text)
          (message "DEBUG %s" (format "%s" elisp-code)))
      (error
       (cd original-dir)
       (llemacs-after-run-hook-error err prompt-text)
       (error "ELMO failed: %s" err)))))

(defun llemacs-after-run-hook-success (elisp-code prompt-text)
  "Hook for successful ELMO operation."
  (llemacs--log-prompt prompt-text)
  (llemacs--log-success (format "Successfully executed:\n%s" elisp-code)))

(defun llemacs-after-run-hook-error (error prompt-text)
  "Hook for failed ELMO operation."
  (llemacs--log-prompt prompt-text)
  (llemacs--log-error (format "Elmo-run failed.\n%s" error))
  (llemacs--log-open))

;; (defun llemacs-run (&optional prompt)
;;   "Run ELMO operations with given PROMPT."
;;   (interactive)
;;   (let ((prompt-text (or prompt (read-string "ELMO prompt: ") ""))
;;         (original-dir default-directory))
;;     (llemacs-before-run-hook prompt-text)
;;     (condition-case err
;;         (let ((elisp-code (llemacs-lang2elisp prompt-text)))
;;           (unless elisp-code
;;             (signal 'error '("No valid elisp code generated")))
;;           (llemacs-get-main-buffer)
;;           (llemacs-exec-local elisp-code)
;;           (cd original-dir)
;;           (message "DEBUG %s" (format "%s" elisp-code)))
;;       (error
;;        (cd original-dir)
;;        (llemacs--log-prompt prompt-text)
;;        (llemacs--log-error (format "Failed to run prompt:\n%s" err))
;;        (error "ELMO failed: %s" err)))))

;; (defun llemacs-after-run-hook-error (error prompt-text)
;;   "Hook for failed ELMO operation."
;;   (llemacs--log-error (format "Elmo-run failed.\n%s\n%s" erro elisp-code))
;;   (llemacs--log-open))


(defalias 'er 'llemacs-run)

(provide '10-llemacs-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))