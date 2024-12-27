;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 19:29:10
;;; Time-stamp: <2024-12-27 19:29:10 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/elisp/elmo/10-elmo-run.el

(require '01-elmo-config)
(require '02-elmo-logging-core)
;; (require '03-elmo-logging-utils)
(require '04-elmo-utils)
(require '09-elmo-lang2elisp)

(defvar elmo-tab-counter 0
  "Counter for ELMO tab numbering.")

(defvar elmo-tab-counter 0
  "Counter for ELMO tab numbering.

Example:
  (setq elmo-tab-counter 0)
  (1+ elmo-tab-counter) ;; => 1")

(defun elmo-before-run-hook (prompt)
  "Prepare environment before running ELMO operations."
  (condition-case err
      (elmo-exec-local
       `(progn
          (elmo-log-note "
================================================================================
Elmo-run called.
================================================================================")
          (elmo-get-main-buffer)
          (setq elmo-original-directory default-directory)
          (setq shell-file-name "/bin/bash")
          (setq python-shell-virtualenv-root "/workspace/.env")
          (setq python-shell-interpreter "/workspace/.env/bin/python3")))
    (error
     (elmo-log-error (format "Failed in before-run hook: %s" err))
     nil)))


(defun elmo-run (&optional prompt)
  "Run ELMO operations with given PROMPT."
  (interactive)
  (let ((prompt-text (or prompt (read-string "ELMO prompt: ") ""))
        (original-dir default-directory))
    (elmo-before-run-hook prompt-text)
    (condition-case err
        (let ((elisp-code (elmo-lang2elisp prompt-text)))
          (unless elisp-code
            (signal 'error '("No valid elisp code generated")))
          (elmo-get-main-buffer)
          (elmo-exec-local elisp-code)
          (cd original-dir)
          (message "DEBUG %s" (format "%s" elisp-code)))
      (error
       (cd original-dir)
       (elmo-log-prompt prompt-text)
       (elmo-log-error (format "Failed to run prompt:\n%s" err))
       (error "ELMO failed: %s" err)))))

;; (defun elmo-run (&optional prompt)
;;   "Run ELMO operations with given PROMPT."
;;   (interactive)
;;   (let ((prompt-text (or prompt (read-string "ELMO prompt: ") "")))
;;     (elmo-before-run-hook prompt-text)
;;     (condition-case err
;;         (let ((elisp-code (elmo-lang2elisp prompt-text)))
;;           (unless elisp-code
;;             (signal 'error '("No valid elisp code generated")))
;;           (elmo-get-main-buffer)
;;           (elmo-exec-local elisp-code)
;;           (message "DEBUG %s" (format "%s" elisp-code))
;;       (error
;;        (elmo-log-prompt prompt-text)
;;        (elmo-log-error (format "Failed to run prompt:\n%s" err))
;;        (error "ELMO failed: %s" err)))))

(defun elmo-after-run-hook-error (error prompt-text)
  "Hook for failed ELMO operation."
  (elmo-log-error (format "Elmo-run failed.\n%s\n%s" erro elisp-code))
  (elmo-log-open))


(defalias 'er 'elmo-run)

(provide '10-elmo-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))