;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:32:35
;;; Time-stamp: <2024-12-06 00:32:35 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-python.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-06 00:32:33
;; ;;; Time-stamp: <2024-12-06 00:32:33 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/llemacs-python.el


;; (require 'llemacs-config)
;; (require 'llemacs-logging)

;; (defun llemacs-setup-python-env ()
;;   (interactive)
;;   (condition-case err
;;       (progn
;;         (unless (file-exists-p "/home/llemacs/.env")
;;           (make-directory "/home/llemacs/.env" t)
;;           (shell-command "python -m venv /home/llemacs/.env")
;;           (sleep-for 1))
;;         (setq python-shell-virtualenv-root "/home/llemacs/.env"
;;               python-shell-interpreter "/home/llemacs/.env/bin/python3"
;;               org-babel-python-command "/home/llemacs/.env/bin/python3"))
;;     (error
;;      (llemacs-log-error (format "Python setup failed: %s" (error-message-string err)))
;;      nil)))

;; (provide 'llemacs-python)


;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
