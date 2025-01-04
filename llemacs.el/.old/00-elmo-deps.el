;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 23:14:28
;;; Time-stamp: <2024-12-24 23:14:28 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/llemacs/elisp/llemacs/00-llemacs-deps.el

;; External dependencies
(require 'json)
(require 'request)
(require 'w3m nil t)

;; Internal dependencies order
(defvar llemacs-module-order
  '(01-llemacs-config
    02-llemacs-logging-core
    03-llemacs-logging-utils
    04-llemacs-utils
    05-llemacs-image
    06-llemacs-json-md
    07-llemacs-exec
    08-llemacs-prompt-templates
    ;; 09-llemacs-network
    ;; 10-llemacs-lang2elisp
    ;; 11-llemacs-run
    ))

(defun llemacs-load-modules ()
  "Load ELMO modules in correct order."
  (dolist (module llemacs-module-order)
    (require module)))

(provide '00-llemacs-deps)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))