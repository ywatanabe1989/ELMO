;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-04 09:37:29
;;; Time-stamp: <2024-12-04 09:37:29 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-prompts.el


(require 'ninja-config)
(require 'ninja-logging)

(defvar ninja-available-prompts '("authorities" "lang2elisp" "logging" "notes")
  "List of available prompt names without .md extension.")



(defun ninja-load-markdown-file (file-path)
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
     (ninja--log-message (format "Failed to load markdown file %s: %s" file-path err))
     nil)))
;; (ninja-get-prompt "authorities" "logging")

;; (defun ninja-load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string."
;;   (interactive)
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (buffer-string))
;;     (error
;;      (ninja--log-message (format "Failed to load markdown file %s: %s" file-path err))
;;      nil)))

(defun ninja-get-prompt (&rest prompt-names)
  "Get concatenated contents of PROMPT-NAMES markdown files."
  (interactive)
  (condition-case err
      (let ((contents ""))
        (dolist (name prompt-names)
          (unless (member name ninja-available-prompts)
            (ninja--log-message (format "Invalid prompt name: %s" name))
            (error "Invalid prompt name: %s" name))
          (let ((content (ninja-load-markdown-file
                         (expand-file-name (format "%s.md" name) ninja-prompts-dir))))
            (when content
              (setq contents (concat contents "\n" content)))))
        (string-trim contents))
    (error
     (ninja--log-message (format "Error getting prompts: %s" err))
     nil)))
;; (ninja-get-prompt "authorities" "logging")

(provide 'ninja-prompts)

;; ;;; -*- lexical-binding: t -*-
;; ;;; Author: 2024-12-04 08:36:15
;; ;;; Time-stamp: <2024-12-04 08:36:15 (ywatanabe)>
;; ;;; File: ./self-evolving-agent/src/ninja-prompts.el


;;   ;; /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/src/prompts:
;;   ;; drwxrwsr-x 3 ywatanabe ninja 4.0K Dec  4 08:32 .
;;   ;; drwxr-xr-x 5 ywatanabe ninja 4.0K Dec  4 08:18 ..
;;   ;; -rw-r--r-- 1 ywatanabe ninja  937 Dec  4 08:31 authorities.md
;;   ;; -rw-r--r-- 1 ywatanabe ninja 1.4K Dec  4 08:32 lang2elisp.md
;;   ;; -rw-r--r-- 1 ywatanabe ninja  874 Dec  4 08:27 logging.md
;;   ;; -rw-r--r-- 1 ywatanabe ninja  658 Dec  4 08:32 notes.md

;; (defun ninja-load-markdown-file (file-path)
;;   "Load contents of markdown FILE-PATH as string."
;;   (interactive)
;;   (condition-case err
;;       (with-temp-buffer
;;         (insert-file-contents file-path)
;;         (buffer-string))
;;     (error
;;      (ninja--log-error (format "Failed to load markdown file %s: %s" file-path err))
;;      nil)))

;; (defun ninja-get-prompt (prompt-name)
;;   (interactive)
;;   (ninja-load-markdown-file (expand-file-name (format "%s.md" prompt-name) ninja-prompts-dir)))

;;

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
