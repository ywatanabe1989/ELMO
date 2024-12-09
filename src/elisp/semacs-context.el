;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 09:35:21
;;; Time-stamp: <2024-12-04 09:35:21 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-context.el

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-04 08:55:51
;; ;;; Time-stamp: <2024-12-04 08:55:51 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/ninja-context.el


;; (defun ninja--get-context ()
;;   "Gather current context including buffer state and history."
;;   (interactive)
;;   (setq ninja-context
;;         `((history . ,(ninja--read-history)))))
;; ;; (ninja--get-context)

;; (provide 'ninja-context)

;; 

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
