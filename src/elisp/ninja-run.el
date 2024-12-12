;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:44:22
;;; Time-stamp: <2024-12-06 05:44:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-run.el


(require 'ninja-config)
(require 'ninja-lang2elisp)
(require 'ninja-python)
(require 'ninja-logging)

(defun ninja-before-run-hook ()
  "Prepare environment before running NINJA operations."
  (condition-case err
      (ninja-exec-elisp-code
      '(progn
        (tab-new)
        (tab-rename (format-time-string "NINJA-%Y-%m-%d %H:%M:%S"))
        (generate-new-buffer "*ninja-output*")
        (switch-to-buffer "*ninja-output*")
        (setq default-directory "/home/ninja")
        (setq shell-file-name "/bin/bash"
              python-shell-virtualenv-root "/home/ninja/.env"
              python-shell-interpreter "/home/ninja/.env/bin/python3"
              org-babel-python-command "/home/ninja/.env/bin/python3")
        )
      )
    (error
     (ninja--log-error (format "Failed in before-run hook: %s" err))
     nil)))


(defun ninja-run (&optional prompt)
  (interactive)
  (let ((prompt-text (or prompt (read-string "NINJA prompt: "))))
    (ninja-before-run-hook)
    (condition-case err
        (let ((elisp-code (ninja--prompt-to-elisp prompt-text)))
          (if elisp-code
              (progn
                (ninja-exec-elisp-code elisp-code)
                (ninja--log-success "Successfully executed prompt")
                (ninja--log-del-errors))
            (progn
              (ninja--log-error "No valid elisp code generated from prompt")
              (ninja--log-command-error elisp-code))))
      (error
       (ninja--log-error (format "Failed to run prompt: %s" err))
       nil))))

(defalias 'sr 'ninja-run)

;; (ninja-run "write hello world in python")
;; (ninja-run "write simple python code")


;; (defun ninja--add-timestamp-suffix (name)
;;   (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
;;     (if (string-match "\\.\\([^.]*\\)$" name)
;;         (replace-match (concat "-" timestamp "." (match-string 1 name)) t t name)
;;       (concat name "-" timestamp))))




;; (defun ninja-run (prompt)
;;   "Run NINJA prompt asynchronously."
;;   (interactive)
;;   (run-with-idle-timer
;;    0 nil
;;    (lambda (p)
;;      (condition-case err
;;          (let ((elisp-code (ninja--prompt-to-elisp p)))
;;            (if elisp-code
;;                (ninja-exec-elisp-code elisp-code)
;;              (ninja--log-error "No valid elisp code generated from prompt")))
;;        (error
;;         (ninja--log-error (format "Failed to run prompt: %s" err))
;;         nil)))
;;    prompt))

(provide 'ninja-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
