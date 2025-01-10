;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:56:46
;;; Timestamp: <2025-01-11 08:56:46>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/03-run-progn.el

(defun llemacs--run-progn (prompt)
  "Run PROMPT absed on the 'code-elisp-progn` recipe"
  (llemacs--run-prompt prompt "code-elisp-progn"))

;; (llemacs--run-progn "plot something")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
