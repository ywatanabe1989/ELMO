;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-25 00:35:37
;;; Time-stamp: <2024-12-25 00:35:37 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/elmo.el

(defvar elmo-lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing ELMO's Elisp files.")

(add-to-list 'load-path elmo-lisp-dir)
;; ;; User installation paths
;; (defvar elmo-user-root-dir
;;   (file-name-directory (directory-file-name
;;                         (file-name-directory
;;                          (or load-file-name buffer-file-name))))
;;   "User's ELMO installation root directory.")
;; ;; "/home/ywatanabe/.emacs.d/lisp/elmo/elisp/"

;; (defvar elmo-user-source-dir
;;   (expand-file-name "src" elmo-user-root-dir)
;;   "User's ELMO source directory.")

;; (add-to-list 'load-path elmo-user-root-dir)
;; (add-to-list 'load-path elmo-user-source-dir)

;; Load base configuration first
(require '01-elmo-config)
(require '02-elmo-logging-core)
(require '03-elmo-logging-utils)
;; Load remaining components after ensuring installation
(require '04-elmo-utils)
(require '05-elmo-image)
(require '06-elmo-json-md)
(require '07-elmo-exec)
(require '08-elmo-prompt-templates)
(require '09-elmo-lang2elisp)
(require '10-elmo-run)
;; (require '11-elmo-run)

;; ;; Load base configuration first
;; (require 'elmo-config)
;; (require 'elmo-logging-core)
;; (require 'elmo-logging-utils)

;; ;; Load remaining components after ensuring installation
;; (require 'elmo-utils)
;; (require 'elmo-image)
;; (require 'elmo-json-md)
;; (require 'elmo-exec)
;; (require 'elmo-prompt-templates)


;; ;; (require 'elmo-network)
;; ;; (require 'elmo-lang2elisp)
;; ;; (require 'elmo-run)


(provide 'elmo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))