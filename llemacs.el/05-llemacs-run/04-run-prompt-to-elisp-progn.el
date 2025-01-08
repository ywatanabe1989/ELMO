;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 07:37:10
;;; Timestamp: <2025-01-09 07:37:10>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/04-run-prompt-to-elisp-progn.el

(defun llemacs--run-prompt-to-progn (prompt)
  (llemacs--run-prompt prompt "code-elisp-progn"))

;; (message "from here")
;; (llemacs--run-prompt "plot something" "code-elisp-progn")
;; (llemacs--run-prompt-to-progn "plot something")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
