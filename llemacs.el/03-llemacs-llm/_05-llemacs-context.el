;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 12:45:41
;;; Time-stamp: <2024-12-31 12:45:41 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/05-llemacs-context.el

(require '06-llemacs-compress)


(defun llemacs-context-init (project-id)
  "Initialize project context."
  (let ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id))))
    (unless (file-exists-p context-dir)
      (make-directory context-dir t))
    (with-temp-file (expand-file-name "init.json" context-dir)
      (insert (json-encode-pretty
               `((project-id . ,project-id)
                 (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
                 (status . "initialized")))))))

(defun llemacs-context-update (project-id data)
  "Update context with DATA."
  (let* ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id)))
         (context-file (expand-file-name
                        (format "context-%s.json"
                                (format-time-string "%Y%m%d-%H%M%S"))
                        context-dir)))
    (with-temp-file context-file
      (insert (json-encode-pretty data)))))

(defun llemacs-context-get-latest (project-id)
  "Get latest context data."
  (let* ((context-dir (expand-file-name "context" (llemacs-project-get-dir project-id)))
         (files (directory-files context-dir t "context-.*\\.json$")))
    (when files
      (with-temp-buffer
        (insert-file-contents (car (last files)))
        (json-read-from-string (buffer-string))))))

;; (defun llemacs-context-add-compressed-output (project-id step-name output)
;;   "Add compressed OUTPUT for STEP-NAME in PROJECT-ID."
;;   (let ((compressed (llemacs-compress-text output 1000)))
;;     (llemacs-context-add-output project-id step-name compressed)))

;; (defun llemacs-context-compress-memory (project-id)
;;   "Compress memory contents of PROJECT-ID."
;;   (let ((memory-dir (expand-file-name "memory"
;;                                       (llemacs-project-get-dir project-id))))
;;     (llemacs-compress-dir memory-dir "\\.json$" 500)))

;; (defun llemacs-context-load-compressed-memory (project-id)
;;   "Load compressed memory contents for PROJECT-ID."
;;   (let ((compressed-dir (expand-file-name "compressed"
;;                                           (expand-file-name "memory"
;;                                                             (llemacs-project-get-dir project-id)))))
;;     (when (file-exists-p compressed-dir)
;;       (mapcar (lambda (file)
;;                 (with-temp-buffer
;;                   (insert-file-contents file)
;;                   (buffer-string)))
;;               (directory-files compressed-dir t "comp-.*\\.json$")))))

(provide '05-llemacs-context)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))