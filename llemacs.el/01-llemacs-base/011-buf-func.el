;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 18:12:33
;;; Time-stamp: <2025-01-04 18:12:33 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/011-buf-func.el

(require 'org)

(defun llemacs--buf-disp (buffer &optional file-path enable-q enable-readonly enable-org)
  "Display BUFFER with specified parameters.

Example:
(llemacs--buf-disp \"*test*\" \"~/test.txt\" t t t)

Arguments:
BUFFER - Buffer name to display
FILE-PATH - Optional path to file to insert
ENABLE-Q - Enable 'q' key for quitting window
ENABLE-READONLY - Make buffer read-only
ENABLE-ORG - Enable org-mode"
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
          (org-mode)))
      (when enable-readonly
        (read-only-mode 1))))
  (display-buffer buffer '(display-buffer-pop-up-window))
  (with-selected-window (get-buffer-window buffer)
    (goto-char (point-max))
    (recenter -1)))

(defmacro llemacs--def-buf-disp (name sys-var pj-var)
  "Define buffer display functions for system and project contexts.
NAME is the base name (e.g., main, logging).
SYS-VAR and PJ-VAR are the corresponding buffer name variables."
  `(progn
     (defun ,(intern (format "llemacs--buf-disp-%s-sys" name))
         (&optional file-path enable-q enable-readonly enable-org)
       ,(format "Display system-wide %s buffer.\nOptional FILE-PATH specifies content to load." name)
       (llemacs--buf-disp ,sys-var file-path enable-q enable-readonly enable-org ))

     (defun ,(intern (format "llemacs--buf-disp-%s-pj" name))
         (&optional file-path enable-q enable-readonly enable-org)
       ,(format "Display project-specific %s buffer.\nOptional FILE-PATH specifies content to load." name)
       (llemacs--buf-disp ,pj-var file-path enable-q enable-readonly enable-org ))))

(llemacs--def-buf-disp "main" llemacs--buf-main-sys llemacs--buf-main-pj)
(llemacs--def-buf-disp "logging" llemacs--buf-logging-sys llemacs--buf-logging-pj)
(llemacs--def-buf-disp "debug" llemacs--buf-debug-sys llemacs--buf-debug-pj)
(llemacs--def-buf-disp "prompt" llemacs--buf-prompt-sys llemacs--buf-prompt-pj)
(llemacs--def-buf-disp "search" llemacs--buf-search-sys llemacs--buf-search-pj)

;; (llemacs--buf-disp-debug-pj nil )

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))