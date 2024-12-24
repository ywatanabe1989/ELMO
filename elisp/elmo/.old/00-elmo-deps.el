;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 23:14:28
;;; Time-stamp: <2024-12-24 23:14:28 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/00-elmo-deps.el

;; External dependencies
(require 'json)
(require 'request)
(require 'w3m nil t)

;; Internal dependencies order
(defvar elmo-module-order
  '(01-elmo-config
    02-elmo-logging-core
    03-elmo-logging-utils
    04-elmo-utils
    05-elmo-image
    06-elmo-json-md
    07-elmo-exec
    08-elmo-prompt-templates
    ;; 09-elmo-network
    ;; 10-elmo-lang2elisp
    ;; 11-elmo-run
    ))

(defun elmo-load-modules ()
  "Load ELMO modules in correct order."
  (dolist (module elmo-module-order)
    (require module)))

(provide '00-elmo-deps)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))