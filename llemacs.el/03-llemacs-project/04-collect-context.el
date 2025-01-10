;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:29:42
;;; Timestamp: <2025-01-11 08:29:42>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-project/04-collect-context.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defun llemacs--pj-get-path-log (full-pj-name &optional log-level)
  "Get path to project's log file. If LOG-LEVEL is nil, return the main log file."
  (let* ((project-dir (llemacs--pj-get-dir full-pj-name))
         (level (if (stringp log-level)
                    (intern log-level)
                  log-level)))
    (expand-file-name
     (if level
         (pcase level
           ('debug "logs/by_level/debug.log")
           ('elisp "logs/by_level/elisp.log")
           ('error "logs/by_level/error.log")
           ('info "logs/by_level/info.log")
           ('prompt "logs/by_level/prompt.log")
           ('search "logs/by_level/search.log")
           ('warn "logs/by_level/warn.log"))
       "logs/all.log")
     project-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents Readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--safe-read-file (path)
  "Safely read file contents from PATH."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents-literally path)
        (replace-regexp-in-string "\r\n" "\n" (buffer-string)))
    (llemacs--logging-write-error-pj "")))

(defun llemacs--pj-get-contents (path)
  "Read contents of FILE safely."
  (when (and path (file-exists-p path))
    (llemacs--safe-read-file path)))

(defun llemacs--pj-get-contents-pm-mmd ()
  "Read contents of the mermaid file for project management."
  (llemacs--pj-get-contents llemacs--path-pj-pm-mmd))

(defun llemacs--pj-get-contents-log (full-pj-name &optional log-level)
  "Read contents of project's log file."
  (llemacs--pj-get-contents
   (llemacs--pj-get-path-log full-pj-name log-level)))

(defun llemacs--pj-get-contents-tree (full-pj-name)
  "Get tree-like directory structure of the project."
  (let ((project-dir (llemacs--pj-get-dir full-pj-name)))
    (condition-case err
        (replace-regexp-in-string "\r\n" "\n"
                                  (shell-command-to-string
                                   (format "tree -a --charset ascii --gitignore -L 3 %s" project-dir)))
      (llemacs--logging-write-error-pj ""))))

;; Main
(defun llemacs--format-log-entry (log-entry)
  "Format a log entry, ensuring multi-line content is properly handled."
  (let ((level (car log-entry))
        (content (cdr log-entry)))
    (format "----------------------------------------\n[%s LOG]\n%s\n"
            (upcase (symbol-name level))
            (replace-regexp-in-string "\n" "\n" content)))) ; Ensure newlines are preserved


;; ;; working but encoding problems
;; (defun llemacs--pj-collect-context (full-pj-name &optional log-level)
;;   "Collect all relevant context information for the project."
;;   (let* ((log-level (if (stringp log-level) (intern log-level) log-level))
;;          (log-level (or log-level 'error))
;;          (all-logs (list (cons log-level (llemacs--pj-get-contents-log full-pj-name log-level))))
;;          (limited-logs (seq-take (reverse all-logs) llemacs--log-entry-limit))
;;          (tree (llemacs--pj-get-contents-tree full-pj-name))
;;          (content (concat
;;                    llemacs--project-splitter
;;                    "##### CONTEXT STARTS #####"
;;                    llemacs--project-splitter
;;                    (format "Current Timestamp:\n%s" (or (llemacs-timestamp) "N/A"))
;;                    llemacs--project-splitter
;;                    (format "Current Project Directory:\n%s" (or llemacs--path-pj "N/A"))
;;                    llemacs--project-splitter
;;                    (format "Current Project Management (mermaid):\n%s\n%s"
;;                            (or llemacs--path-pj-pm-mmd "N/A")
;;                            (or (llemacs--pj-get-contents-pm-mmd) "N/A"))
;;                    llemacs--project-splitter
;;                    "Current Project Logs:\n"
;;                    (mapconcat #'llemacs--format-log-entry limited-logs)
;;                    llemacs--project-splitter
;;                    (format "Current Project Structure:\n%s" (or tree "N/A"))
;;                    llemacs--project-splitter
;;                    "##### CONTEXT ENDS #####")))
;;     (format "%s\nTotal Characters: %d" content (length content))))

;; Excluding non-ASCII/UTF-8 characters
(defun llemacs--pj-collect-context (full-pj-name &optional log-level)
  "Collect all relevant context information for the project."
  (let* ((log-level (if (stringp log-level) (intern log-level) log-level))
         (log-level (or log-level 'error))
         (all-logs (list (cons log-level (llemacs--pj-get-contents-log full-pj-name log-level))))
         (limited-logs (seq-take (reverse all-logs) llemacs--log-entry-limit))
         (tree (llemacs--pj-get-contents-tree full-pj-name))
         (content (replace-regexp-in-string
                   "[^\x00-\xFF]+" ""
                   (concat
                    llemacs--project-splitter
                    "##### CONTEXT STARTS #####"
                    llemacs--project-splitter
                    (format "Current Timestamp:\n%s" (or (llemacs-timestamp) "N/A"))
                    llemacs--project-splitter
                    (format "Current Project Directory:\n%s" (or llemacs--path-pj "N/A"))
                    llemacs--project-splitter
                    (format "Current Project Management (mermaid):\n%s\n%s"
                            (or llemacs--path-pj-pm-mmd "N/A")
                            (or (llemacs--pj-get-contents-pm-mmd) "N/A"))
                    llemacs--project-splitter
                    "Current Project Logs:\n"
                    (mapconcat #'llemacs--format-log-entry limited-logs)
                    llemacs--project-splitter
                    (format "Current Project Structure:\n%s" (or tree "N/A"))
                    llemacs--project-splitter
                    "##### CONTEXT ENDS #####"))))
    (format "%s\nTotal Characters: %d" content (length content))))

;; (defun llemacs--pj-collect-context (full-pj-name &optional log-level)
;;   "Collect all relevant context information for the project."
;;   (let* ((log-level (if (stringp log-level) (intern log-level) log-level)) ; Convert string to symbol
;;          (log-level (or log-level 'error)) ; Default to 'error if log-level is nil
;;          (all-logs (list (cons log-level (llemacs--pj-get-contents-log full-pj-name log-level))))
;;          (limited-logs (seq-take (reverse all-logs) llemacs--log-entry-limit))
;;          (tree (llemacs--pj-get-contents-tree full-pj-name)))
;;     (concat
;;      llemacs--project-splitter
;;      "##### CONTEXT STARTS #####"
;;      llemacs--project-splitter
;;      (format "Current Timestamp:\n%s" (or (llemacs-timestamp) "N/A"))
;;      llemacs--project-splitter
;;      (format "Current Project Directory:\n%s" (or llemacs--path-pj "N/A"))
;;      llemacs--project-splitter
;;      (format "Current Project Management (mermaid):\n%s\n%s"
;;              (or llemacs--path-pj-pm-mmd "N/A")
;;              (or (llemacs--pj-get-contents-pm-mmd) "N/A"))
;;      llemacs--project-splitter
;;      "Current Project Logs:\n"
;;      (mapconcat #'llemacs--format-log-entry limited-logs)
;;      llemacs--project-splitter
;;      (format "Current Project Structure:\n%s" (or tree "N/A"))
;;      llemacs--project-splitter
;;      "##### CONTEXT ENDS #####"
;;      )))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
