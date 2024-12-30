;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-28 18:57:37
;;; Time-stamp: <2024-12-28 18:57:37 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/12-llemacs-context.el

(require '10-llemacs-compressor)

(defun llemacs-context-add-compressed-output (project-id step-name output)
  "Add compressed OUTPUT for STEP-NAME in PROJECT-ID."
  (let ((compressed (llemacs-compress-text output 1000)))
    (llemacs-context-add-output project-id step-name compressed)))

(defun llemacs-context-compress-memory (project-id)
  "Compress memory contents of PROJECT-ID."
  (let ((memory-dir (expand-file-name "memory"
                                      (llemacs-project-get-dir project-id))))
    (llemacs-compress-dir memory-dir "\\.json$" 500)))

(defun llemacs-context-load-compressed-memory (project-id)
  "Load compressed memory contents for PROJECT-ID."
  (let ((compressed-dir (expand-file-name "compressed"
                                          (expand-file-name "memory"
                                                            (llemacs-project-get-dir project-id)))))
    (when (file-exists-p compressed-dir)
      (mapcar (lambda (file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)))
              (directory-files compressed-dir t "comp-.*\\.json$")))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))