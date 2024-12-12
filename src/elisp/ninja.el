;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:26:34
;;; Time-stamp: <2024-12-10 16:51:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja.el


;;(require 'ninja)

;; User installation paths
(defvar ninja-user-root-dir (file-name-directory (directory-file-name
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))
  "User's NINJA installation root directory.")

(defvar ninja-user-source-dir (expand-file-name "src" ninja-user-root-dir)
  "User's NINJA source directory.")

(add-to-list 'load-path ninja-user-root-dir)
(add-to-list 'load-path ninja-user-source-dir)

;; Load base configuration first
(require 'ninja-config)
(require 'ninja-logging)

;; Load components in order
(require 'ninja-install)
(require 'ninja-verify-installation)

;; Load remaining components after ensuring installation
(require 'ninja-prompts)
(require 'ninja-mode)
(require 'ninja-utils)
(require 'ninja-version-control)
(require 'ninja-network)
(require 'ninja-run)
(require 'ninja-self-evolve)
(require 'ninja-server)
(require 'ninja-lang2elisp)
(require 'ninja-run)
(require 'ninja-python)

(remove-hook 'after-change-functions #'genai-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))


(provide 'ninja)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
