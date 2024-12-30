;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 16:45:41
;;; Time-stamp: <2024-12-29 16:45:41 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/14-llemacs-search.el

(require 'json)
(require 'url)

(defvar llemacs-brave-key (getenv "BRAVE_API_KEY")
  "API key for Brave Search.")

(defun llemacs-search-brave (query)
  "Search the web using the Brave Search API."
  (let ((url "https://api.search.brave.com/res/v1/web/search")
        (url-request-extra-headers `(("X-Subscription-Token" . ,llemacs-brave-key)))
        (url-request-method "GET")
        (url-request-data (format "q=%s" (url-hexify-string query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (json-read)))))

(defun llemacs-search-brave-results (query)
  "Retrieve and display Brave Search results."
  (let ((results (llemacs-search-brave query)))
    (mapcar (lambda (result)
              (cons (alist-get 'title result)
                    (alist-get 'url result)))
            (alist-get 'results (alist-get 'web results)))))

;; Example usage
(llemacs-search-brave-results "latest LLM models deepseek")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))