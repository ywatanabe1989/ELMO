;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 10:56:41
;;; Time-stamp: <2024-12-02 10:56:41 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-mode.el


(defvar elmo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'elmo-cleanup-buffer)
    map)
  "Keymap for ELMO mode.")

(define-derived-mode elmo-mode special-mode "ELMO"
  "Major mode for ELMO interaction."
  (use-local-map elmo-mode-map))

(provide 'elmo-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
