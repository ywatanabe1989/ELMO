;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 23:06:23
;;; Time-stamp: <2024-12-24 23:06:23 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/01-elmo-config.el

(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup elmo nil
  "Self-evolving agent configuration."
  :group 'applications)

(defcustom elmo-workspace-dir
  "/home/ywatanabe/.emacs.d/lisp/elmo/workspace"
  "ELMO working directory."
  :type 'directory
  :group 'elmo)

(defcustom elmo-id
  "001"
  "ELMO ID"
  :type 'string
  :group 'elmo)

(defcustom elmo-home
  (expand-file-name (format "elmos/elmo-%s" elmo-id) elmo-workspace-dir)
  "ELMO user home directory."
  :type 'directory
  :group 'elmo)

(defcustom elmo-server
  (expand-file-name ".emacs.d/emacs-server/server" elmo-home)
  "ELMO emacs server."
  :type 'directory
  :group 'elmo)

(provide '01-elmo-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))