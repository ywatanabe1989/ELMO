;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 10:56:41
;;; Time-stamp: <2024-12-02 10:56:41 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-mode.el


(defvar semacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b") 'semacs-cleanup-buffer)
    map)
  "Keymap for SEMACS mode.")

(define-derived-mode semacs-mode special-mode "SEMACS"
  "Major mode for SEMACS interaction."
  (use-local-map semacs-mode-map))

(provide 'semacs-mode)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
