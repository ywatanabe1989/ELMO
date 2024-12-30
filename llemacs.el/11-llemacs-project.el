;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:33:35
;;; Time-stamp: <2024-12-29 08:33:35 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/11-llemacs-project.el

(defvar llemacs-project-dir
  (expand-file-name "projects" llemacs-path-workspace)
  "Directory for ELMO projects.")

(defvar llemacs-project-status-options
  '("planning" "in-progress" "review" "completed" "archived")
  "Available status options for ELMO projects.")

(defvar llemacs-project-markdown-template
  "# Project: {name}
## Overview
- ID: {id}
- Version: {version}
- Status: {status}
## Objective
{objective}
## Success Criteria
{criteria}
## Implementation
{steps}
## Notes
{notes}
"
  "Template for project metadata in markdown format.")

(defvar llemacs-project-metadata-template
  '(:version "0.0.0"
             :id nil
             :name nil
             :status "planning"
             :objective "Write main objective here..."
             :criteria "List success criteria here..."
             :steps "Describe implementation steps here..."
             :notes "Add any additional notes, risks, or dependencies here...")
  "Template for project metadata in JSON format.")

(defvar llemacs-project-id-file
  (expand-file-name ".project-id" llemacs-project-dir)
  "File to store the latest project ID.")


(defun llemacs-project-verify-json (json-string)
  "Verify if JSON-STRING is valid project metadata."
  (condition-case err
      (let ((data (json-read-from-string json-string)))
        (and (alist-get 'id data)
             (alist-get 'name data)
             (alist-get 'version data)
             (alist-get 'status data)
             (alist-get 'objective data)
             (alist-get 'criteria data)
             (alist-get 'steps data)
             (alist-get 'notes data)))
    (error nil)))


(defun llemacs-project-next-id ()
  "Get next available project ID."
  (let ((id 1))
    (when (file-exists-p llemacs-project-id-file)
      (with-temp-buffer
        (insert-file-contents llemacs-project-id-file)
        (setq id (1+ (string-to-number (buffer-string))))))
    (with-temp-file llemacs-project-id-file
      (insert (number-to-string id)))
    (format "%03d" id)))


(defun llemacs-project-generate-markdown (json-data)
  "Convert JSON-DATA to markdown using template."
  (let* ((data (if (stringp json-data)
                   (json-read-from-string json-data)
                 json-data))
         (md llemacs-project-markdown-template))
    (dolist (pair '(("name" . "name")
                    ("id" . "id")
                    ("version" . "version")
                    ("status" . "status")
                    ("objective" . "objective")
                    ("criteria" . "criteria")
                    ("steps" . "steps")
                    ("notes" . "notes")))
      (setq md (replace-regexp-in-string
                (format "{%s}" (car pair))
                (or (alist-get (cdr pair) data nil nil #'string=) "")
                md)))
    md))

(defun llemacs-project-generate-json (md-content)
  "Parse markdown MD-CONTENT to JSON format."
  (let ((data (copy-tree llemacs-project-metadata-template)))
    (with-temp-buffer
      (insert md-content)
      (goto-char (point-min))
      (while (re-search-forward "^##? \\(.+\\)$" nil t)
        (let ((section (match-string 1)))
          (forward-line)
          (let ((content (buffer-substring (point)
                                           (or (re-search-forward "^##? " nil t)
                                               (point-max)))))
            (pcase section
              ("Overview"
               (when (string-match "ID: \\(.+\\)" content)
                 (setq data (plist-put data :id (match-string 1 content))))
               (when (string-match "Version: \\(.+\\)" content)
                 (setq data (plist-put data :version (match-string 1 content)))))
              ("Objective" (setq data (plist-put data :objective content)))
              ("Success Criteria" (setq data (plist-put data :criteria content)))
              ("Implementation" (setq data (plist-put data :steps content)))
              ("Notes" (setq data (plist-put data :notes content))))))))
    (json-encode-pretty data)))


(defun llemacs-project-ensure-structure (project-id project-name)
  "Ensure project structure exists for PROJECT-ID with PROJECT-NAME."
  (let* ((dir-name (format "%s-%s" project-id project-name))
         (project-dir (expand-file-name dir-name llemacs-project-dir))
         (context-dir (expand-file-name "context" project-dir))
         (steps-dir (expand-file-name "steps" project-dir))
         (memory-dir (expand-file-name "memory" project-dir))
         (metadata-json (expand-file-name "metadata.json" project-dir))
         (metadata-md (expand-file-name "metadata.md" project-dir)))
    (dolist (dir (list project-dir context-dir steps-dir memory-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (unless (file-exists-p metadata-json)
      (with-temp-file metadata-json
        (let ((json-encoding-pretty-print t)
              (data (copy-tree llemacs-project-metadata-template)))
          (setq data (plist-put data :id project-id))
          (setq data (plist-put data :name project-name))
          (setq data (plist-put data :status (car llemacs-project-status-options)))
          (insert (json-encode data))))
      (with-temp-file metadata-md
        (insert (llemacs-project-generate-markdown
                 (with-temp-buffer
                   (insert-file-contents metadata-json)
                   (buffer-string))))))))


(defun llemacs-project-edit-metadata (&optional project-id)
  "Edit project metadata in a dedicated buffer for PROJECT-ID."
  (interactive)
  (let* ((project-id (or project-id
                         (completing-read "Project ID: "
                                          (directory-files llemacs-project-dir nil "^[0-9]+"))))
         (project-dir-pattern (format "^%s-.*$" project-id))
         (project-dirname (car (directory-files llemacs-project-dir nil project-dir-pattern)))
         (project-name (if project-dirname
                           (substring project-dirname (+ 1 (length project-id)))
                         "new-project"))
         (project-dir (if project-dirname
                          (expand-file-name project-dirname llemacs-project-dir)
                        (expand-file-name (format "%s-%s" project-id project-name) llemacs-project-dir)))
         (metadata-json (expand-file-name "metadata.json" project-dir))
         (metadata-md (expand-file-name "metadata.md" project-dir))
         (buf (get-buffer-create (format "*ELMO Project %s*" project-id))))
    (unless (file-exists-p metadata-json)
      (llemacs-project-ensure-structure project-id project-name))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents metadata-json)
      (js-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let ((content (buffer-string)))
                         (if (llemacs-project-verify-json content)
                             (progn
                               (with-temp-file metadata-json
                                 (insert content))
                               (with-temp-file metadata-md
                                 (insert (llemacs-project-generate-markdown content)))
                               (message "Project metadata saved successfully.")
                               (kill-buffer))
                           (message "Invalid metadata format. Please check the content.")))))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (kill-buffer))))
    (switch-to-buffer buf)
    (message "Press C-c C-c to save, C-c C-k to cancel")))


(defun llemacs-project-edit-markdown (project-id)
  "Edit markdown metadata for PROJECT-ID."
  (interactive)
  (let* ((project-dir-pattern (format "^%s-.*$" project-id))
         (project-dirname (car (directory-files llemacs-project-dir nil project-dir-pattern)))
         (project-dir (expand-file-name project-dirname llemacs-project-dir))
         (metadata-md (expand-file-name "metadata.md" project-dir))
         (metadata-json (expand-file-name "metadata.json" project-dir)))
    (find-file metadata-md)
    (when (= (buffer-size) 0)
      (insert llemacs-project-markdown-template))
    (add-hook 'after-save-hook
              (lambda ()
                (when (string= (buffer-file-name) metadata-md)
                  (with-temp-file metadata-json
                    (insert (llemacs-project-generate-json (buffer-string))))))
              nil t)
    (markdown-mode)))


(defun llemacs-project-create ()
  "Create new ELMO project."
  (interactive)
  (let* ((project-name (read-string "Project Name: "))
         (project-id (llemacs-project-next-id)))
    (llemacs-project-ensure-structure project-id project-name)
    (llemacs-project-edit-metadata project-id)))


;; Entry Point:
;; (llemacs-project-create)
;; (llemacs-project-edit-metadata)

(provide '11-llemacs-project-handler)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))