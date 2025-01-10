;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 09:50:54
;;; Timestamp: <2025-01-11 09:50:54>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/_99-run-auto.el

(require 'async)

;; Mermaid
(defun llemacs-pj-update-mermaid ()
  (interactive)
  (llemacs-run-project
   "- Update the project_management.mmd (`llemacs--path-pj-pm-mmd`) based on the goals and progress so far
- Carefully review tags in the mermaid file (`todo`, `inProgress`, and `done`). This is critical as this file is only the communication tool between agents and users. So, `done` tag is only assigned when the task is certainly compeleted.
- For example, to add `done` tag, at lest one representative figure is representative figure.
- It is the worst case where the `done` tag is applied even when the task is not completed. This will cause other agents to skip such tasks. So, please carefully scrutinize progress tags.
- Split into small milestones and tasks, which consolidate the base of successful achievement of goals.
- Directory tree and past logs will be especially useful.
- Render the updated prorject_management.mmd as png and svg images.
- Summarize the images into report with org mode
- Open the created svg file using `$ wslview` or `$ google-chrome`
- Demo data should be saved under (`llemacs--path-pj-data`) for reuse
"
   "project-management"
   "error"))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
