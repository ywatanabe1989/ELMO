;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:32:35
;;; Time-stamp: <2024-12-06 00:32:35 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-python.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-06 00:32:33
;; ;;; Time-stamp: <2024-12-06 00:32:33 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/elmo-python.el


;; (require 'elmo-config)
;; (require 'elmo-logging)

;; (defun elmo-setup-python-env ()
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (unless (file-exists-p "/home/elmo/.env")
;;           (make-directory "/home/elmo/.env" t)
;;           (shell-command "python -m venv /home/elmo/.env")
;;           (sleep-for 1))
;;         (setq python-shell-virtualenv-root "/home/elmo/.env"
;;               python-shell-interpreter "/home/elmo/.env/bin/python3"
;;               org-babel-python-command "/home/elmo/.env/bin/python3"))
;;     (error
;;      (elmo-log-error (format "Python setup failed: %s" (error-message-string err)))
;;      nil)))

;; (provide 'elmo-python)


;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
