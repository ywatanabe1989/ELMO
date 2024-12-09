;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 10:56:41
;;; Time-stamp: <2024-12-02 10:56:41 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-mode.el


(defvar sea-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'sea-cleanup-buffer)
    map)
  "Keymap for SEA mode.")

(define-derived-mode sea-mode special-mode "SEA"
  "Major mode for SEA interaction."
  (use-local-map sea-mode-map))

(provide 'sea-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
