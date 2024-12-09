;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:32:35
;;; Time-stamp: <2024-12-06 00:32:35 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-python.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-06 00:32:33
;; ;;; Time-stamp: <2024-12-06 00:32:33 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/sea-python.el


;; (require 'sea-config)
;; (require 'sea-logging)

;; (defun sea-setup-python-env ()
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (unless (file-exists-p "/home/sea/.env")
;;           (make-directory "/home/sea/.env" t)
;;           (shell-command "python -m venv /home/sea/.env")
;;           (sleep-for 1))
;;         (setq python-shell-virtualenv-root "/home/sea/.env"
;;               python-shell-interpreter "/home/sea/.env/bin/python3"
;;               org-babel-python-command "/home/sea/.env/bin/python3"))
;;     (error
;;      (sea--log-error (format "Python setup failed: %s" (error-message-string err)))
;;      nil)))

;; (provide 'sea-python)


;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
