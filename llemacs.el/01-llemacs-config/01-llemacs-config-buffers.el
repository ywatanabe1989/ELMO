;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 20:14:42
;;; Time-stamp: <2024-12-31 20:14:42 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-config/01-llemacs-config-buffers.el

;; ----------------------------------------
;; Buffers
;; ----------------------------------------
;; Core Buffers
(defcustom llemacs--buffer-main "*LLEMACS*"
  "Name of main buffer."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-log "*LLEMACS-LOGGING*"
  "Name of log buffer."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-debug "*LLEMACS-DEBUG*"
  "Buffer for debug output when debug mode enabled."
  :type 'string
  :group 'llemacs-buffer)

;; Feature-specific Buffers
(defcustom llemacs--buffer-project "*LLEMACS-PROJECT*"
  "Buffer for project operations."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-search "*LLEMACS-SEARCH*"
  "Buffer for search results."
  :type 'string
  :group 'llemacs-buffer)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))