;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:54:04
;;; Time-stamp: <2025-01-02 10:54:04 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/_context-xxx.el

(defun llemacs--llm-context-compile (project-id))
(defun llemacs--llm-context-get-goals (project-id))
(defun llemacs--llm-context-get-milestones (project-id))
(defun llemacs--llm-context-get-tasks (project-id))
(defun llemacs--llm-context-get-workspace (project-id))

;; (defun llemacs-context-init (project-id)
;;   "Initialize project context."
;;   (let ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id))))
;;     (unless (file-exists-p context-dir)
;;       (make-directory context-dir t))
;;     (with-temp-file (expand-file-name "init.json" context-dir)
;;       (insert (json-encode-pretty
;;                `((project-id . ,project-id)
;;                  (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
;;                  (status . "initialized")))))))

;; (defun llemacs-context-update (project-id data)
;;   "Update context with DATA."
;;   (let* ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id)))
;;          (context-file (expand-file-name
;;                         (format "context-%s.json"
;;                                 (format-time-string "%Y%m%d-%H%M%S"))
;;                         context-dir)))
;;     (with-temp-file context-file
;;       (insert (json-encode-pretty data)))))

;; (defun llemacs-context-get-latest (project-id)
;;   "Get latest context data."
;;   (let* ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id)))
;;          (files (directory-files context-dir t "context-.*\\.json$")))
;;     (when files
;;       (with-temp-buffer
;;         (insert-file-contents (car (last files)))
;;         (json-read-from-string (buffer-string))))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))