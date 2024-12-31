;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:23:11
;;; Time-stamp: <2024-12-31 23:23:11 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/05-llemacs-run/05-llemacs-helper-functions.el

(defun llemacs--escape-elisp-code (code)
  "Escape elisp CODE for shell execution."
  (condition-case err
      (prin1-to-string code)
    (error
     (llemacs--logging-error (format "Code escaping failed: %s" (error-message-string err)))
     nil)))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))