;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:20:27
;;; Time-stamp: <2025-01-06 17:20:27 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/002-mermaid-save-hook.el

(defun llemacs--mermaid-compile ()
  "Generate PNG, SVG, and GIF after saving Mermaid files."
  (interactive)
  (when (and (eq major-mode 'mermaid-mode)
             (buffer-file-name))
    (let* ((input-file (buffer-file-name))
           (base-name (file-name-sans-extension input-file)))
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".png")
                    "--backgroundColor" "transparent")
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".svg")
                    "--backgroundColor" "transparent"
                    "-f" "svg")
      (call-process "convert" nil nil nil
                    (concat base-name ".png")
                    (concat base-name ".gif"))
      (message "Created %s.png, %s.svg and %s.gif" base-name base-name base-name))))

(defun llemacs--mermaid-after-save-hook ()
  (llemacs--mermaid-compile))

(use-package mermaid-mode
  :ensure t
  :custom
  (setq mermaid-output-format ".png")
  :hook
  (after-save . llemacs--mermaid-after-save-hook))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path "/usr/local/bin/mmdc")
  (ob-mermaid-extra-args "--backgroundColor transparent -f svg"))

(org-babel-do-load-languages
 'org-babel-load-languages
