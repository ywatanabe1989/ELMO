;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 09:37:29
;;; Time-stamp: <2024-12-04 09:37:29 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-prompts.el


(require 'semacs-config)
(require 'semacs-logging)

(defvar semacs-available-prompts '("authorities" "lang2elisp" "logging" "notes")
  "List of available prompt names without .md extension.")



(defun semacs-load-markdown-file (file-path)
  "Load contents of markdown FILE-PATH as string, skipping metadata comments."
  (interactive)
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (when (looking-at "<!--[^>]*-->")
          (goto-char (match-end 0))
          (forward-line))
        (buffer-substring-no-properties (point) (point-max)))
    (error
     (semacs--log-message (format "Failed to load markdown file %s: %s" file-path err))
     nil)))
;; (semacs-get-prompt "authorities" "logging")

;; (defun semacs-load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string."
;;   (interactive)
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (buffer-string))
;;     (error
;;      (semacs--log-message (format "Failed to load markdown file %s: %s" file-path err))
;;      nil)))

(defun semacs-get-prompt (&rest prompt-names)
  "Get concatenated contents of PROMPT-NAMES markdown files."
  (interactive)
  (condition-case err
      (let ((contents ""))
        (dolist (name prompt-names)
          (unless (member name semacs-available-prompts)
            (semacs--log-message (format "Invalid prompt name: %s" name))
            (error "Invalid prompt name: %s" name))
          (let ((content (semacs-load-markdown-file
                         (expand-file-name (format "%s.md" name) semacs-prompts-dir))))
            (when content
              (setq contents (concat contents "\n" content)))))
        (string-trim contents))
    (error
     (semacs--log-message (format "Error getting prompts: %s" err))
     nil)))
;; (semacs-get-prompt "authorities" "logging")

(provide 'semacs-prompts)

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-04 08:36:15
;; ;;; Time-stamp: <2024-12-04 08:36:15 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/semacs-prompts.el


;;   ;; /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/src/prompts:
;;   ;; drwxrwsr-x 3 ywatanabe semacs 4.0K Dec  4 08:32 .
;;   ;; drwxr-xr-x 5 ywatanabe semacs 4.0K Dec  4 08:18 ..
;;   ;; -rw-r--r-- 1 ywatanabe semacs  937 Dec  4 08:31 authorities.md
;;   ;; -rw-r--r-- 1 ywatanabe semacs 1.4K Dec  4 08:32 lang2elisp.md
;;   ;; -rw-r--r-- 1 ywatanabe semacs  874 Dec  4 08:27 logging.md
;;   ;; -rw-r--r-- 1 ywatanabe semacs  658 Dec  4 08:32 notes.md

;; (defun semacs-load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string."
;;   (interactive)
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (buffer-string))
;;     (error
;;      (semacs--log-error (format "Failed to load markdown file %s: %s" file-path err))
;;      nil)))

;; (defun semacs-get-prompt (prompt-name)
;;   (interactive)
;;   (semacs-load-markdown-file (expand-file-name (format "%s.md" prompt-name) semacs-prompts-dir)))

;;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
