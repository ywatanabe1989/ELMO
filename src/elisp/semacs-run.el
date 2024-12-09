;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:44:22
;;; Time-stamp: <2024-12-06 05:44:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-run.el


(require 'semacs-config)
(require 'semacs-lang2elisp)
(require 'semacs-python)
(require 'semacs-logging)

(defun semacs-before-run-hook ()
  "Prepare environment before running SEMACS operations."
  (condition-case err
      (semacs-exec-elisp-code
      '(progn
        (tab-new)
        (tab-rename (format-time-string "SEMACS-%Y-%m-%d %H:%M:%S"))
        (generate-new-buffer "*semacs-output*")
        (switch-to-buffer "*semacs-output*")
        (setq default-directory "/home/semacs")
        (setq shell-file-name "/bin/bash"
              python-shell-virtualenv-root "/home/semacs/.env"
              python-shell-interpreter "/home/semacs/.env/bin/python3"
              org-babel-python-command "/home/semacs/.env/bin/python3")
        )
      )
    (error
     (semacs--log-error (format "Failed in before-run hook: %s" err))
     nil)))


(defun semacs-run (&optional prompt)
  (interactive)
  (let ((prompt-text (or prompt (read-string "SEMACS prompt: "))))
    (semacs-before-run-hook)
    (condition-case err
        (let ((elisp-code (semacs--prompt-to-elisp prompt-text)))
          (if elisp-code
              (progn
                (semacs-exec-elisp-code elisp-code)
                (semacs--log-success "Successfully executed prompt")
                (semacs--log-del-errors))
            (progn
              (semacs--log-error "No valid elisp code generated from prompt")
              (semacs--log-command-error elisp-code))))
      (error
       (semacs--log-error (format "Failed to run prompt: %s" err))
       nil))))

(defalias 'sr 'semacs-run)

;; (semacs-run "write hello world in python")
;; (semacs-run "write simple python code")


;; (defun semacs--add-timestamp-suffix (name)
;;   (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
;;     (if (string-match "\\.\\([^.]*\\)$" name)
;;         (replace-match (concat "-" timestamp "." (match-string 1 name)) t t name)
;;       (concat name "-" timestamp))))




;; (defun semacs-run (prompt)
;;   "Run SEMACS prompt asynchronously."
;;   (interactive)
;;   (run-with-idle-timer
;;    0 nil
;;    (lambda (p)
;;      (condition-case err
;;          (let ((elisp-code (semacs--prompt-to-elisp p)))
;;            (if elisp-code
;;                (semacs-exec-elisp-code elisp-code)
;;              (semacs--log-error "No valid elisp code generated from prompt")))
;;        (error
;;         (semacs--log-error (format "Failed to run prompt: %s" err))
;;         nil)))
;;    prompt))

(provide 'semacs-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
