;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 10:56:41
;;; Time-stamp: <2024-12-02 10:56:41 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-mode.el


(defvar ninja-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'ninja-cleanup-buffer)
    map)
  "Keymap for NINJA mode.")

(define-derived-mode ninja-mode special-mode "NINJA"
  "Major mode for NINJA interaction."
  (use-local-map ninja-mode-map))

(provide 'ninja-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
