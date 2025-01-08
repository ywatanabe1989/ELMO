;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 21:37:53
;;; Timestamp: <2025-01-08 21:37:53>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-prompt-to-elisp-progn.el

(defun llemacs--run-prompt-to-progn (prompt)
  (llemacs--run-prompt prompt "code-elisp-progn"))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
