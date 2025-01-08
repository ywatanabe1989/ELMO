;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:24:26
;;; Time-stamp: <2025-01-06 20:24:26 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-lemacs-secrets/03-ssh.el

(defun llemacs--store-ssh-key (name key)
  (llemacs--secrets-store
   (format "ssh_key_%s" name)
   key))

(defun llemacs--get-ssh-key (name)
  (llemacs--secrets-get
   (format "ssh_key_%s" name)))
