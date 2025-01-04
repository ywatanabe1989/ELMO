;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:54:48
;;; Time-stamp: <2025-01-02 10:54:48 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/07-llemacs-tools/_99-llemacs-search.el

(require 'json)
(require 'url)

;; ----------------------------------------
;; Search Configuration
;; ----------------------------------------
(defcustom llemacs-search-backend 'ripgrep
  "Default backend for searching."
  :type '(choice (const :tag "Ripgrep" ripgrep)
                 (const :tag "Grep" grep)
                 (const :tag "AG" ag))
  :group 'llemacs-search)

(defcustom llemacs-search-max-results 1000
  "Maximum number of search results to display."
  :type 'integer
  :group 'llemacs-search)

(defcustom llemacs-search-context-lines 2
  "Number of context lines to show around matches."
  :type 'integer
  :group 'llemacs-search)

(defcustom llemacs-search-ignore-case t
  "Whether to ignore case in searches by default."
  :type 'boolean
  :group 'llemacs-search)

(defcustom llemacs-search-exclude-patterns
  '(".git" ".svn" "node_modules" "*.pyc" "*.elc")
  "Patterns to exclude from searches."
  :type '(repeat string)
  :group 'llemacs-search)

(defvar llemacs--search-brave-key (getenv "BRAVE_API_KEY")
  "API key for Brave Search.")

(defun llemacs--search-brave (query)
  "Search the web using the Brave Search API."
  (let ((url "https://api.search.brave.com/res/v1/web/search")
        (url-request-extra-headers `(("X-Subscription-Token" . ,llemacs--search-brave-key)))
        (url-request-method "GET")
        (url-request-data (format "q=%s" (url-hexify-string query))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (json-read)))))

(defun llemacs-search (query)
  "Retrieve and display Brave Search results for QUERY."
  (let ((results (llemacs--search-brave query)))
    (mapcar (lambda (result)
              (cons (alist-get 'title result)
                    (alist-get 'url result)))
            (alist-get 'results (alist-get 'web results)))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-30 17:23:00
;; ;;; Time-stamp: <2024-12-30 17:23:00 (ywatanabe)>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/14-llemacs-search.el

;; (require 'json)
;; (require 'url)

;; (defvar llemacs-brave-key (getenv "BRAVE_API_KEY")
;;   "API key for Brave Search.")

;; (defun llemacs-search-brave (query)
;;   "Search the web using the Brave Search API."
;;   (let ((url "https://api.search.brave.com/res/v1/web/search")
;;         (url-request-extra-headers `(("X-Subscription-Token" . ,llemacs-brave-key)))
;;         (url-request-method "GET")
;;         (url-request-data (format "q=%s" (url-hexify-string query))))
;;     (with-current-buffer (url-retrieve-synchronously url)
;;       (goto-char (point-min))
;;       (re-search-forward "^$")
;;       (let ((json-object-type 'alist)
;;             (json-array-type 'list)
;;             (json-key-type 'symbol))
;;         (json-read)))))

;; (defun llemacs-search-brave-results (query)
;;   "Retrieve and display Brave Search results."
;;   (let ((results (llemacs-search-brave query)))
;;     (mapcar (lambda (result)
;;               (cons (alist-get 'title result)
;;                     (alist-get 'url result)))
;;             (alist-get 'results (alist-get 'web results)))))

;; ;; Example usage
;; (insert (llemacs-search-brave-results "latest LLM models deepseek"))

;; (provide '14-llemacs-search)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))