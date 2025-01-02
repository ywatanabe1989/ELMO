;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 04:52:59
;;; Time-stamp: <2025-01-03 04:52:59 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-proj/proj-collect-context.el

(defun llemacs--proj-get-path-pm-mmd (project-id-or-full-project-name)
  "Get path to project's pm.mmd file."
  (let ((project-dir (llemacs--proj-get-dir project-id-or-full-project-name)))
    (expand-file-name "pm/pm.mmd" project-dir)))
;; (llemacs--proj-get-path-pm-mmd "026-my-first-project")

(defun llemacs--proj-get-path-log (project-id-or-full-project-name &optional log-level)
  "Get path to project's log file. If LOG-LEVEL is nil, return the main log file."
  (let* ((project-dir (llemacs--proj-get-dir project-id-or-full-project-name))
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
;; (llemacs--proj-get-path-log "026-my-first-project" "warn")
;; (llemacs--proj-get-path-log "026-my-first-project" 'warn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents Readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--proj-get-contents (path)
  "Read contents of FILE."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun llemacs--proj-get-contents-pm-mmd (project-id-or-full-project-name)
  "Read contents of project's pm.mmd file."
  (llemacs--proj-get-contents
   (llemacs--proj-get-path-pm-mmd project-id-or-full-project-name)))
;; (llemacs--proj-get-contents-pm-mmd "026-my-first-project")

(defun llemacs--proj-get-contents-log (project-id-or-full-project-name &optional log-level)
  "Read contents of project's log file."
  (llemacs--proj-get-contents
   (llemacs--proj-get-path-log project-id-or-full-project-name log-level)))
;; (llemacs--proj-get-contents-log "026-my-first-project" 'warn)

(defun llemacs--proj-get-tree-contents (project-id-or-full-project-name)
  "Get tree-like directory structure of the project."
  (let ((project-dir (llemacs--proj-get-dir project-id-or-full-project-name)))
    (shell-command-to-string
     (format "tree -a --charset ascii --gitignore %s" project-dir))))
;; (llemacs--proj-get-tree-contents "026-my-first-project")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--proj-collect-context (project-id-or-full-project-name &optional log-level)
  "Collect all relevant context information for the project."
  (let* ((project-dir (llemacs--proj-get-dir project-id-or-full-project-name))
         (pm-mmd-path (llemacs--proj-get-path-pm-mmd project-id-or-full-project-name))
         (pm-mmd (llemacs--proj-get-contents pm-mmd-path))
         (all-logs (if log-level
                       (list (cons log-level (llemacs--proj-get-contents-log project-id-or-full-project-name log-level)))
                     (mapcar (lambda (level)
                               (cons level (llemacs--proj-get-contents-log project-id-or-full-project-name level)))
                             '(debug elisp error info prompt search warn))))
         (tree (llemacs--proj-get-tree-contents project-id-or-full-project-name)))
    (concat
     llemacs--project-splitter
     "##### CONTEXT STARTS #####"
     llemacs--project-splitter
     (format "\nCurrent Timestamp:\n%s\n\n" (llemacs--logging-get-timestamp))
     llemacs--project-splitter
     (format "\nProject Directory:\n%s\n\n" project-dir)
     llemacs--project-splitter
     (format "\nProject Management (%s):\n%s\n\n" pm-mmd-path pm-mmd)
     llemacs--project-splitter
     (mapconcat (lambda (log-entry)
                  (let ((level (car log-entry))
                        (content (cdr log-entry)))
                    (format "(%s):\n%s\n%s\n"
                            level
                            (llemacs--proj-get-path-log project-id-or-full-project-name level)
                            content)))
                all-logs
                "\n")
     llemacs--project-splitter
     "##### CONTEXT ENDS #####"
     llemacs--project-splitter
     (format "\nProject Structure:\n%s" tree))))

;; (llemacs--proj-collect-context "036-my-project")
;; (llemacs--cvt-prompt2elisp (llemacs--proj-collect-context "036-my-project"))
;; (llemacs--run-prompt (llemacs--proj-collect-context "036-my-project"))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))