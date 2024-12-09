;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:32:35
;;; Time-stamp: <2024-12-06 00:32:35 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-python.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-06 00:32:33
;; ;;; Time-stamp: <2024-12-06 00:32:33 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/semacs-python.el


;; (require 'semacs-config)
;; (require 'semacs-logging)

;; (defun semacs-setup-python-env ()
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (unless (file-exists-p "/home/semacs/.env")
;;           (make-directory "/home/semacs/.env" t)
;;           (shell-command "python -m venv /home/semacs/.env")
;;           (sleep-for 1))
;;         (setq python-shell-virtualenv-root "/home/semacs/.env"
;;               python-shell-interpreter "/home/semacs/.env/bin/python3"
;;               org-babel-python-command "/home/semacs/.env/bin/python3"))
;;     (error
;;      (semacs--log-error (format "Python setup failed: %s" (error-message-string err)))
;;      nil)))

;; (provide 'semacs-python)


;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
