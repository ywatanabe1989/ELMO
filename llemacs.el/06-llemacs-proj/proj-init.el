;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 02:30:01
;;; Time-stamp: <2025-01-03 02:30:01 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/06-llemacs-project/project_init.el

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

(defun llemacs--proj-get-dir (project-id-or-full-project-name)
  "Get project directory for PROJECT-ID-OR-FULL-PROJECT-NAME."
  (unless project-id-or-full-project-name
    (llemacs--logging-log-error "Project ID/name cannot be nil")
    (return-from llemacs--proj-get-dir nil))

  (unless (file-exists-p llemacs--path-projects)
    (llemacs--logging-log-error "Projects directory does not exist")
    (return-from llemacs--proj-get-dir nil))

  (let* ((is-full-name (string-match-p "-" project-id-or-full-project-name))
         (project-id (if is-full-name
                         (car (split-string project-id-or-full-project-name "-"))
                       project-id-or-full-project-name))
         (pattern (format "^%s-.*$" project-id)))

    (if-let ((dirname (car (directory-files llemacs--path-projects nil pattern))))
        (progn
          ;; (llemacs--logging-log-success
          ;;  (format "Found project directory: %s" dirname))
          (expand-file-name dirname llemacs--path-projects))
      (llemacs--logging-log-error
       (format "No project found for ID/name: %s" project-id-or-full-project-name))
      nil)))
;; (llemacs--proj-get-dir "026-my-first-project")


;; ;; Preparation for the template project
;; cd ./workspace/projects/
;; unzip 000-sample-project.zip
;; zip -r 000-sample-project.zip 000-sample-project/

(defun llemacs--proj-init-pm (project-path project-name project-goals)
  "Initialize project management file with provided details."
  (find-file (expand-file-name "pm/pm.mmd" project-path))
  (goto-char (point-min))
  (while (search-forward "PJNAME[Project Name]" nil t)
    (replace-match (format "PJNAME[%s]" project-name)))
  (goto-char (point-min))
  (while (search-forward "PJGOALS[Goals]" nil t)
    (replace-match (format "PJGOALS[%s]" project-goals)))
  (goto-char (point-min))
  (while (search-forward "\\/workspace\\/projects\\/000-sample-project" nil t)
    (replace-match project-path)))


(defun llemacs--proj-init (project-name &optional goals)
  "Create new project with basic structure."
  (interactive "sProject Name: ")
  (let* ((project-id (llemacs--project-get-next-id))
         (project-full-name (format "%s-%s" project-id project-name))
         (project-path (expand-file-name project-full-name llemacs--path-projects))
         (project-goals (or goals (read-string "Project Goals: "))))
    (when (file-exists-p project-path)
      (llemacs--logging-log-error "Project directory already exists")
      (return-from llemacs--proj-init nil))
    (if (file-exists-p llemacs--path-sample-project-zip)
        (progn
          (let ((default-directory llemacs--path-projects))
            (call-process "unzip" nil nil nil llemacs--path-sample-project-zip)
            (rename-file "000-sample-project" project-full-name)
            (llemacs--logging-log-success
             (format "Project initialized: %s\nProject directory: %s"
                     project-full-name
                     (llemacs--proj-get-dir project-full-name)))
            (llemacs--proj-init-pm project-path project-name project-goals))
          project-id)
      (llemacs--logging-log-error "Project template not found")
      nil)))

;; (llemacs--proj-init "my-project" "plot something and summarize all the all the code and results as a org file, and open it as a buffer, with inline images displayed")

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))