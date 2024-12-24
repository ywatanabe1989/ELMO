;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-25 01:44:42
;;; Time-stamp: <2024-12-25 01:44:42 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/10-elmo-run.el

(require '01-elmo-config)
(require '02-elmo-logging-core)
(require '03-elmo-logging-utils)
(require '09-elmo-lang2elisp)

(defvar elmo-tab-counter 0
  "Counter for ELMO tab numbering.")


;; (defun elmo-before-run-hook ()
;;   "Prepare environment before running ELMO operations."
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (let ((current-buf (current-buffer))
;;               (buffer (get-buffer-create "*ELMO*")))
;;           (display-buffer buffer
;;                           '((display-buffer-in-side-window)
;;                             (side . right)
;;                             (window-width . 80)))
;;           (set-buffer buffer))
;;         (elmo-exec
;;          `(progn
;;             (setq default-directory "/workspace")
;;             (setq shell-file-name "/bin/bash"
;;                   python-shell-virtualenv-root "/workspace.env"
;;                   python-shell-interpreter "/workspace/.env/bin/python3"))))
;;     (error
;;      (elmo-log-error (format "Failed in before-run hook: %s" err))
;;      nil)))
(defun elmo-before-run-hook ()
  "Prepare environment before running ELMO operations."
  (interactive)
  (condition-case err
      (progn
        (let ((current-buf (current-buffer))
              (buffer (get-buffer-create "*ELMO*")))
          ;; (delete-other-windows)
          (split-window-right)
          (other-window 1)
          (switch-to-buffer buffer)
          (set-buffer buffer))
        (elmo-exec
         `(progn
            (setq default-directory "/workspace")
            (setq shell-file-name "/bin/bash"
                  python-shell-virtualenv-root "/workspace.env"
                  python-shell-interpreter "/workspace/.env/bin/python3"))))
    (error
     (elmo-log-error (format "Failed in before-run hook: %s" err))
     nil)))

(defun elmo-run (&optional prompt)
  (interactive)
  (let ((prompt-text (or prompt (read-string "ELMO prompt: "))))
    (elmo-before-run-hook)
    (condition-case err
        (let ((elisp-code (elmo-lang2elisp prompt-text)))
          (if elisp-code
              (progn
                (elmo-exec elisp-code)
                (elmo-log-success "Successfully executed prompt")
                (elmo-log-del-errors))
            (progn
              (elmo-log-error "No valid elisp code generated from prompt")
              (elmo-log-command-error elisp-code))))
      (error
       (elmo-log-error (format "Failed to run prompt: %s" err))
       nil))))

(defalias 'er 'elmo-run)

(provide '10-elmo-run)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))