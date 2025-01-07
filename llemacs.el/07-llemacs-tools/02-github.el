;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-07 07:55:49
;;; Time-stamp: <2025-01-07 07:55:49 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/08-llemacs-git/github.el

;; GitHub login function
(defun llemacs--github-login ()
  "Login to GitHub using token from environment."
  (let ((token-path (getenv "LLEMACS_GITHUB_TOKEN_PATH")))
    (when token-path
      (let ((token (with-temp-buffer
                     (insert-file-contents token-path)
                     (string-trim (buffer-string)))))
        (llemacs--store-access-token "github" token)
        (llemacs--ensure-gh-auth)))))
