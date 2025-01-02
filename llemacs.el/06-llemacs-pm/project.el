;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 01:29:20
;;; Time-stamp: <2025-01-03 01:29:20 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-pm/project.el

(defun llemacs--project-get-next-id ()
  "Get next available project ID."
  (let ((id 1))
    (when (file-exists-p llemacs--path-project-id)
      (with-temp-buffer
        (insert-file-contents llemacs--path-project-id)
        (setq id (1+ (string-to-number (buffer-string))))))
    (with-temp-file llemacs--path-project-id
      (insert (number-to-string id)))
    (format "%03d" id)))

(defun llemacs--proj-init (project-name)
  "Create new project with basic structure."
  (interactive "sProject Name: ")
  (let* ((project-id (llemacs--project-get-next-id))
         (project-full-name (format "%s-%s" project-id project-name))
         (project-path (expand-file-name project-full-name llemacs-path-projects)))

    (when (file-exists-p project-path)
      (llemacs--logging-log-error "Project directory already exists")
      (return-from llemacs--proj-init nil))

    (if (file-exists-p llemacs--path-sample-project-zip)
        (progn
          (let ((default-directory llemacs-path-projects))
            (call-process "unzip" nil nil nil llemacs--path-sample-project-zip)
            (rename-file "000-sample-project" project-full-name)
            (llemacs--logging-log-success
             (format "Project initialized: %s" project-full-name)))
          project-id)
      (llemacs--logging-log-error "Project template not found")
      nil)))

;; cd ./workspace/projects/
;; unzip 000-sample-project.zip
;; zip -r 000-sample-project.zip 000-sample-project/
;; (llemacs--proj-init "my-first-project")


(defun llemacs--proj-get-dir (project-id-or-full-project-name)
  "Get project directory for PROJECT-ID-OR-FULL-PROJECT-NAME."
  (unless project-id-or-full-project-name
    (llemacs--logging-log-error "Project ID/name cannot be nil")
    (return-from llemacs--proj-get-dir nil))

  (unless (file-exists-p llemacs-path-projects)
    (llemacs--logging-log-error "Projects directory does not exist")
    (return-from llemacs--proj-get-dir nil))

  (let* ((is-full-name (string-match-p "-" project-id-or-full-project-name))
         (project-id (if is-full-name
                         (car (split-string project-id-or-full-project-name "-"))
                       project-id-or-full-project-name))
         (pattern (format "^%s-.*$" project-id)))

    (if-let ((dirname (car (directory-files llemacs-path-projects nil pattern))))
        (progn
          (llemacs--logging-log-success
           (format "Found project directory: %s" dirname))
          (expand-file-name dirname llemacs-path-projects))
      (llemacs--logging-log-error
       (format "No project found for ID/name: %s" project-id-or-full-project-name))
      nil)))
;; (llemacs--proj-get-dir "026-my-first-project")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))