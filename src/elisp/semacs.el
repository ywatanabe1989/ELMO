;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:26:34
;;; Time-stamp: <2024-12-06 00:26:34 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs.el


;;(require 'semacs)

;; User installation paths
(defvar semacs-user-root-dir (file-name-directory (directory-file-name
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))
  "User's SEMACS installation root directory.")

(defvar semacs-user-source-dir (expand-file-name "src" semacs-user-root-dir)
  "User's SEMACS source directory.")

(add-to-list 'load-path semacs-user-root-dir)
(add-to-list 'load-path semacs-user-source-dir)

;; Load base configuration first
(require 'semacs-config)
(require 'semacs-logging)

;; Load components in order
(require 'semacs-install)
(require 'semacs-verify-installation)

;; Load remaining components after ensuring installation
;; (require 'semacs-context)
(require 'semacs-prompts)
(require 'semacs-mode)
(require 'semacs-utils)
(require 'semacs-version-control)
(require 'semacs-network)
(require 'semacs-run)
(require 'semacs-self-evolve)
(require 'semacs-server)
(require 'semacs-lang2elisp)
(require 'semacs-run)
(require 'semacs-python)

(remove-hook 'after-change-functions #'genai-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))


(provide 'semacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
