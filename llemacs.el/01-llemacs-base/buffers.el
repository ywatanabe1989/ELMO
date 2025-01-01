;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 02:50:10
;;; Time-stamp: <2025-01-02 02:50:10 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/01-llemacs-base/buffers.el

(require 'org)

(defvar llemacs--tab-counter 0
  "Counter for LLEMACS tab numbering."  )

(defcustom llemacs--buffer-main "*LLEMACS*"
  "Name of main buffer."
  :type 'string
  :group 'llemacs-buffer)

(defcustom llemacs--buffer-logging "*LLEMACS-LOGGING*"
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

;; (defun llemacs--buffer-display (buffer &optional file-path enable-q enable-org)
;;   "Display BUFFER as popup."
;;   (get-buffer-create buffer)
;;   (with-current-buffer buffer
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (when file-path
;;         (insert-file-contents file-path))
;;       (when enable-q
;;         (local-set-key "q" 'quit-window))
;;       (when enable-org
;;         (unless (eq major-mode 'org-mode)
;;           (org-mode))))
;;     (end-of-buffer))
;;   (display-buffer buffer '(display-buffer-pop-up-window)))


(defun llemacs--buffer-display (buffer &optional file-path enable-q enable-org)
  "Display BUFFER as popup."
  (get-buffer-create buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when file-path
        (insert-file-contents file-path))
      (when enable-q
        (local-set-key "q" 'quit-window))
      (when enable-org
        (unless (eq major-mode 'org-mode)
          (org-mode)))))
  (display-buffer buffer '(display-buffer-pop-up-window))
  (with-selected-window (get-buffer-window buffer)
    (goto-char (point-max))
    (recenter -1)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))