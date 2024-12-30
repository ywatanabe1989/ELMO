;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 10:56:41
;;; Time-stamp: <2024-12-02 10:56:41 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-mode.el


(defvar llemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'llemacs-cleanup-buffer)
    map)
  "Keymap for ELMO mode.")

(define-derived-mode llemacs-mode special-mode "ELMO"
  "Major mode for ELMO interaction."
  (use-local-map llemacs-mode-map))

(provide 'llemacs-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
