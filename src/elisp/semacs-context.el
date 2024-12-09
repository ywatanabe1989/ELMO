;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 09:35:21
;;; Time-stamp: <2024-12-04 09:35:21 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-context.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-04 08:55:51
;; ;;; Time-stamp: <2024-12-04 08:55:51 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/semacs-context.el


;; (defun semacs--get-context ()
;;   "Gather current context including buffer state and history."
;;   (interactive)
;;   (setq semacs-context
;;         `((history . ,(semacs--read-history)))))
;; ;; (semacs--get-context)

;; (provide 'semacs-context)

;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
