;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 09:54:55
;;; Time-stamp: <2025-01-04 09:54:55 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/100-paths.el



;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path
  (expand-file-name "../../" (file-name-directory (or load-file-name buffer-file-name)))
  "Base directory for LLEMACS project."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-workspace
  (expand-file-name "workspace" llemacs--path)
  "Base directory for LLEMACS workspace."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-python-env-sys
  (expand-file-name ".env" llemacs--path-workspace)
  "Path to the Python environment used by llemacs.el."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs--script
  :group 'llemacs-sys)

(defcustom llemacs--path-python-sys
  (expand-file-name "bin/python" llemacs--path-python-env-sys)
  "Path to the Python binary used by llemacs.el."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs--script
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-agents
  (expand-file-name "agents" llemacs--path-workspace)
  "Directory for LLEMACS agents."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-agent
  (expand-file-name (user-login-name) llemacs--path-agents)
  "User-specific home directory for LLEMACS."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-agent)

(defcustom llemacs--path-agent-emacs-server
  (expand-file-name ".emacs.d/emacs-server/server" llemacs--path-agent)
  "Path to LLEMACS Emacs server socket."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-logs-sys
  (expand-file-name "logs" llemacs--path-workspace)
  "Directory for LLEMACS system logs."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-backup-sys
  (expand-file-name "backup" llemacs--path-logs-sys)
  "Directory for LLEMACS system logs backup."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-all-sys
  (expand-file-name "all" llemacs--path-logs-sys)
  "File path for LLEMACS system logs."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-logs-by-level-sys
  (expand-file-name "by_level" llemacs--path-logs-sys)
  "Directory for LLEMACS system logs."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defconst llemacs--log-levels-sys
  '((debug   . (0 . "Debug level logs"))
    (info    . (1 . "Information level logs"))
    (success . (1 . "Success level logs"))
    (prompt  . (1 . "Prompt operation logs"))
    (elisp   . (1 . "Elisp execution logs"))
    (api     . (1 . "API interaction logs"))
    (search  . (1 . "Search operation logs"))
    (warn    . (2 . "Warning level logs"))
    (error   . (3 . "Error level logs"))))

(defun llemacs--path-create-log-paths-sys ()
  "Create system-level log path variables based on `llemacs--log-levels-sys'.
The following variables are initialized in default:
- `llemacs--path-logs-debug-sys'
- `llemacs--path-logs-info-sys'
- `llemacs--path-logs-success-sys'
- `llemacs--path-logs-prompt-sys'
- `llemacs--path-logs-elisp-sys'
- `llemacs--path-logs-api-sys'
- `llemacs--path-logs-search-sys'
- `llemacs--path-logs-warn-sys'
- `llemacs--path-logs-error-sys'"

  (dolist (level llemacs--log-levels-sys)
    (let* ((level-name (car level))
           (level-info (cdr level))
           (desc (cdr level-info))
           (var-name (intern (format "llemacs--path-logs-%s-sys" level-name))))
      (eval
       `(defcustom ,var-name
          (expand-file-name ,(format "%s.log" level-name) llemacs--path-logs-by-level-sys)
          ,(format "Path to LLEMACS system %s file." desc)
          :type 'file
          :group 'llemacs--path
          :group 'llemacs-logging
          :group 'llemacs-sys)))))

(llemacs--path-create-log-paths-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-projects
  (expand-file-name "projects" llemacs--path-workspace)
  "Directory for LLEMACS projects."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-sample-project-zip
  (expand-file-name "000-sample-project.zip" llemacs--path-projects)
  "Sample project zip file for initializing project structure."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--path-res
  (expand-file-name "resources" llemacs--path-workspace)
  "Directory for LLEMACS resources."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-templates
  (expand-file-name "templates" llemacs--path-res)
  "Directory for LLEMACS templates."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompts
  (expand-file-name "prompts" llemacs--path-res)
  "Directory for LLEMACS templates."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-compiled
  (expand-file-name "compiled" llemacs--path-res-prompts)
  "Directory for compiled prompt templates."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-components
  (expand-file-name "components" llemacs--path-res-prompts)
  "Directory for prompt template components."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-prompt-recipes
  (expand-file-name "recipes" llemacs--path-res-prompts)
  "Directory for prompt template recipes."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-scripts
  (expand-file-name "scripts" llemacs--path-res)
  "Directory for LLEMACS scripts."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

(defcustom llemacs--path-res-tools
  (expand-file-name "tools" llemacs--path-res)
  "Directory for LLEMACS tools."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-sys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom llemacs--cur-pj nil
  "Currently active project ID in the form of <id>-<project-name>."
  :type 'string
  :group 'llemacs-project)

(defcustom llemacs--path-pj
  (expand-file-name llemacs--cur-pj llemacs--path-projects)
  "Directory for current project."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-lock
  (expand-file-name ".lock" llemacs--path-pj)
  "Lock file for current project."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-config
  (expand-file-name "config" llemacs--path-pj)
  "Directory for current project configuration."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-data
  (expand-file-name "data" llemacs--path-pj)
  "Directory for current project data."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-results
  (expand-file-name "results" llemacs--path-pj)
  "Directory for current project results."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-scripts
  (expand-file-name "scripts" llemacs--path-pj)
  "Directory for current project scripts."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs
  (expand-file-name "logs" llemacs--path-pj)
  "Directory for current project logs."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-backup
  (expand-file-name "backup" llemacs--path-pj-logs)
  "Directory for current project logs backup."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-all
  (expand-file-name "all.log" llemacs--path-pj-logs)
  "File for current project logs."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-pj-logs-by-level
  (expand-file-name "by_level" llemacs--path-pj-logs)
  "Directory for current project logs."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defalias 'llemacs--path-logs-pj 'llemacs--path-pj-logs)
(defalias 'llemacs--path-logs-backup-pj 'llemacs--path-pj-logs-backup)
(defalias 'llemacs--path-logs-all-pj 'llemacs--path-pj-logs-all)
(defalias 'llemacs--path-logs-by-level-pj 'llemacs--path-pj-logs-by-level)

(defconst llemacs--log-levels-pj llemacs--log-levels-sys)


(defun llemacs--path-create-log-paths-pj ()
  "Create project-specific log path variables based on `llemacs--log-levels-pj'.
The following variables and their aliases are created in default:
Original variables:
- `llemacs--path-pj-logs-debug'
- `llemacs--path-pj-logs-info'
- `llemacs--path-pj-logs-success'
- `llemacs--path-pj-logs-prompt'
- `llemacs--path-pj-logs-elisp'
- `llemacs--path-pj-logs-api'
- `llemacs--path-pj-logs-search'
- `llemacs--path-pj-logs-warn'
- `llemacs--path-pj-logs-error'

Aliased as:
- `llemacs--path-logs-debug-pj'
- `llemacs--path-logs-info-pj'
- `llemacs--path-logs-success-pj'
- `llemacs--path-logs-prompt-pj'
- `llemacs--path-logs-elisp-pj'
- `llemacs--path-logs-api-pj'
- `llemacs--path-logs-search-pj'
- `llemacs--path-logs-warn-pj'
- `llemacs--path-logs-error-pj'"
  (dolist (level llemacs--log-levels-pj)
    (let* ((level-name (car level))
           (var-name (intern (format "llemacs--path-pj-logs-%s" level-name)))
           (alias-name (intern (format "llemacs--path-logs-%s-pj" level-name)))
           (path (expand-file-name (format "%s.log" level-name)
                                   (symbol-value 'llemacs--path-pj-logs-by-level))))
      (set var-name path)
      (defalias alias-name var-name))))

(llemacs--path-create-log-paths-pj)

;; (llemacs-list "variable" "^llemacs--path")
;; (llemacs-list "variable" "^llemacs--path-logs")
;; (llemacs-list "variable" "^llemacs--path-pj")


(defun llemacs--path-init-log-paths-pj ()
  "Initialize project-specific log path variables based on `llemacs--log-levels-pj'."
  (dolist (level llemacs--log-levels-pj)
    (let* ((level-name (car level))
           (level-info (cdr level))
           (desc (cdr level-info))
           (var-name (intern (format "llemacs--path-logs-%s-pj" level-name))))
      (eval
       `(defcustom ,var-name
          (expand-file-name ,(format "%s.log" level-name) llemacs--path-pj-logs-by-level)
          ,(format "Path to current project %s file." desc)
          :type 'file
          :group 'llemacs--path
          :group 'llemacs-logging
          :group 'llemacs-project)))))

(llemacs--path-init-log-paths-pj)

(defcustom llemacs--path-pj-pm
  (expand-file-name "pm" llemacs--path-pj)
  "Directory for current project management."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs-project)

(defcustom llemacs--path-python-env-pj
  (expand-file-name ".env" llemacs--path-pj)
  "Python environment path for current project."
  :type 'directory
  :group 'llemacs--path
  :group 'llemacs--script
  :group 'llemacs-sys)

(defcustom llemacs--path-python
  (expand-file-name "bin/python" llemacs--path-pj)
  "Python path for current project."
  :type 'file
  :group 'llemacs--path
  :group 'llemacs--script
  :group 'llemacs-sys)

(defun llemacs--path-update-project-paths ()
  "Update all project-related paths when switching projects."
  (set 'llemacs--path-pj (expand-file-name llemacs--cur-pj llemacs--path-projects))
  (set 'llemacs--path-pj-config (expand-file-name "config" llemacs--path-pj))
  (set 'llemacs--path-pj-data (expand-file-name "data" llemacs--path-pj))
  (set 'llemacs--path-pj-results (expand-file-name "results" llemacs--path-pj))
  (set 'llemacs--path-pj-scripts (expand-file-name "scripts" llemacs--path-pj))
  (set 'llemacs--path-pj-logs (expand-file-name "logs" llemacs--path-pj))
  (set 'llemacs--path-pj-logs-all (expand-file-name "all.log" llemacs--path-pj-logs))
  (set 'llemacs--path-pj-logs-by-level (expand-file-name "by_level" llemacs--path-pj-logs))
  (set 'llemacs--path-pj-pm (expand-file-name "pm" llemacs--path-pj))
  (set 'llemacs--path-python-env-pj (expand-file-name ".env" llemacs--path-pj))
  (set 'llemacs--path-python (expand-file-name "bin/python" llemacs--path-python-env-pj))
  (llemacs--path-create-log-paths-pj))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))