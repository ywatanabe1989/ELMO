;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:23:44
;;; Time-stamp: <2025-01-06 20:23:44 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/10-lemacs-secrets/02-access_tokens.el

(defun llemacs--store-access-token (service token)
  (llemacs--secrets-store
   (format "access_token_%s" service)
   token))

(defun llemacs--get-access-token (service)
  (llemacs--secrets-get
   (format "access_token_%s" service)))
