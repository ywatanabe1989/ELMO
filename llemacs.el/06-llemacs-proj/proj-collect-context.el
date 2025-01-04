;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 01:29:18
;;; Time-stamp: <2025-01-05 01:29:18 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-proj/proj-collect-context.el

(defun llemacs--proj-get-path-log (full-pj-name &optional log-level)
  "Get path to project's log file. If LOG-LEVEL is nil, return the main log file."
  (let* ((project-dir (llemacs--proj-get-dir full-pj-name))
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
       "logs/logging.log")
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
    (error "")))

(defun llemacs--proj-get-contents (path)
  "Read contents of FILE safely."
  (when (and path (file-exists-p path))
    (llemacs--safe-read-file path)))

(defun llemacs--proj-get-contents-pm-mmd ()
  "Read contents of the mermaid file for project management."
  (llemacs--proj-get-contents llemacs--path-pj-pm-mmd))

(defun llemacs--proj-get-contents-log (full-pj-name &optional log-level)
  "Read contents of project's log file."
  (llemacs--proj-get-contents
   (llemacs--proj-get-path-log full-pj-name log-level)))

(defun llemacs--proj-get-contents-tree (full-pj-name)
  "Get tree-like directory structure of the project."
  (let ((project-dir (llemacs--proj-get-dir full-pj-name)))
    (condition-case err
        (replace-regexp-in-string "\r\n" "\n"
                                  (shell-command-to-string
                                   (format "tree -a --charset ascii --gitignore -L 3 %s" project-dir)))
      (error ""))))

;; (defun llemacs--proj-get-contents (path)
;;   "Read contents of FILE."
;;   (with-temp-buffer
;;     (insert-file-contents path)
;;     (buffer-string)))

;; (defun llemacs--proj-get-contents-pm-mmd ()
;;   "Read contents of the mermaid file for project management."
;;   (llemacs--proj-get-contents llemacs--path-pj-pm-mmd))
;; ;; (llemacs--proj-get-contents-pm-mmd)

;; (defun llemacs--proj-get-contents-log (full-pj-name &optional log-level)
;;   "Read contents of project's log file."
;;   (llemacs--proj-get-contents
;;    (llemacs--proj-get-path-log full-pj-name log-level)))
;; ;; (llemacs--proj-get-contents-log "026-my-first-project" 'warn)

;; (defun llemacs--proj-get-contents-tree (full-pj-name)
;;   "Get tree-like directory structure of the project."
;;   (let ((project-dir (llemacs--proj-get-dir full-pj-name)))
;;     (shell-command-to-string
;;      (format "tree -a --charset ascii --gitignore -L 3 %s" project-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--proj-collect-context (full-pj-name &optional log-level)
  "Collect all relevant context information for the project."
  (let* ((all-logs (if log-level
                       (list (cons log-level (llemacs--proj-get-contents-log full-pj-name log-level)))
                     (mapcar (lambda (level)
                               (cons level (llemacs--proj-get-contents-log full-pj-name level)))
                             '(debug elisp error info prompt search warn))))
         (tree (llemacs--proj-get-contents-tree full-pj-name)))
    (concat
     llemacs--project-splitter
     "##### CONTEXT STARTS #####"
     llemacs--project-splitter
     (format "Current Timestamp:\n%s" (llemacs-timestamp))
     llemacs--project-splitter
     (format "Project Directory:\n%s" llemacs--path-pj)
     llemacs--project-splitter
     (format "Project Management (mermaid):\n%s\n%s" llemacs--path-pj-pm-mmd (llemacs--proj-get-contents-pm-mmd))
     llemacs--project-splitter
     "Project Logs:\n"
     (mapconcat (lambda (log-entry)
                  (let ((level (car log-entry))
                        (content (cdr log-entry)))
                    (format "%s" (or content ""))))
                all-logs)
     llemacs--project-splitter
     "##### CONTEXT ENDS #####"
     llemacs--project-splitter
     (format "Project Structure:\n%s" tree)
     llemacs--project-splitter)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))