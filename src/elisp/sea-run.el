;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:44:22
;;; Time-stamp: <2024-12-06 05:44:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-run.el


(require 'sea-config)
(require 'sea-lang2elisp)
(require 'sea-python)
(require 'sea-logging)

(defun sea-before-run-hook ()
  "Prepare environment before running SEA operations."
  (condition-case err
      (sea-exec-elisp-code
      '(progn
        (tab-new)
        (tab-rename (format-time-string "SEA-%Y-%m-%d %H:%M:%S"))
        (generate-new-buffer "*sea-output*")
        (switch-to-buffer "*sea-output*")
        (setq default-directory "/home/sea")
        (setq shell-file-name "/bin/bash"
              python-shell-virtualenv-root "/home/sea/.env"
              python-shell-interpreter "/home/sea/.env/bin/python3"
              org-babel-python-command "/home/sea/.env/bin/python3")
        )
      )
    (error
     (sea--log-error (format "Failed in before-run hook: %s" err))
     nil)))


(defun sea-run (&optional prompt)
  (interactive)
  (let ((prompt-text (or prompt (read-string "SEA prompt: "))))
    (sea-before-run-hook)
    (condition-case err
        (let ((elisp-code (sea--prompt-to-elisp prompt-text)))
          (if elisp-code
              (progn
                (sea-exec-elisp-code elisp-code)
                (sea--log-success "Successfully executed prompt")
                (sea--log-del-errors))
            (progn
              (sea--log-error "No valid elisp code generated from prompt")
              (sea--log-command-error elisp-code))))
      (error
       (sea--log-error (format "Failed to run prompt: %s" err))
       nil))))

(defalias 'sr 'sea-run)

;; (sea-run "write hello world in python")
;; (sea-run "write simple python code")


;; (defun sea--add-timestamp-suffix (name)
;;   (let ((timestamp (format-time-string "%Y%m%d-%H%M%S")))
;;     (if (string-match "\\.\\([^.]*\\)$" name)
;;         (replace-match (concat "-" timestamp "." (match-string 1 name)) t t name)
;;       (concat name "-" timestamp))))




;; (defun sea-run (prompt)
;;   "Run SEA prompt asynchronously."
;;   (interactive)
;;   (run-with-idle-timer
;;    0 nil
;;    (lambda (p)
;;      (condition-case err
;;          (let ((elisp-code (sea--prompt-to-elisp p)))
;;            (if elisp-code
;;                (sea-exec-elisp-code elisp-code)
;;              (sea--log-error "No valid elisp code generated from prompt")))
;;        (error
;;         (sea--log-error (format "Failed to run prompt: %s" err))
;;         nil)))
;;    prompt))

(provide 'sea-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
