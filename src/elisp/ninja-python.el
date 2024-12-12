;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:32:35
;;; Time-stamp: <2024-12-06 00:32:35 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-python.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-06 00:32:33
;; ;;; Time-stamp: <2024-12-06 00:32:33 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/ninja-python.el


;; (require 'ninja-config)
;; (require 'ninja-logging)

;; (defun ninja-setup-python-env ()
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (unless (file-exists-p "/home/ninja/.env")
;;           (make-directory "/home/ninja/.env" t)
;;           (shell-command "python -m venv /home/ninja/.env")
;;           (sleep-for 1))
;;         (setq python-shell-virtualenv-root "/home/ninja/.env"
;;               python-shell-interpreter "/home/ninja/.env/bin/python3"
;;               org-babel-python-command "/home/ninja/.env/bin/python3"))
;;     (error
;;      (ninja--log-error (format "Python setup failed: %s" (error-message-string err)))
;;      nil)))

;; (provide 'ninja-python)


;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
