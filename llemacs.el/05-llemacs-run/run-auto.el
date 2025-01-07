;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-05 19:29:05
;;; Time-stamp: <2025-01-05 19:29:05 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-auto.el

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

;; Development
(defun llemacs-pj-step ()
  (interactive)
  (llemacs-run-project
   "- Advance the project baesd on the project_management.mmd (`llemacs--path-pj-pm-mmd`)
- Plot a representative figure which showcase what the python script does.
- Save python scripts under the scripts dir (`llemacs--path-pj-scripts`).
- Save Elisp code should be saved  under the scripts dir (`llemacs--path-pj-scripts`).
- Save outputs (e.g., jpg figures) under the results dir (`llemacs--path-pj-results`)
- Mermaid tags might be wrong. So, please check the directory tree carefully and determine which task you should work on.
- You can also edit the mermaid file.
- Summarize results as an org report with inline images displayed
- To save jpg image in python, this snippet will be helpful:
``` python
import Image
import matplotlib.pyplot as plt

plt.plot(range(10))
plt.savefig('testplot.png')
Image.open('testplot.png').save('testplot.jpg','JPEG')
```
"
   "code-gen"
   "error"))

(defun llemacs-run-auto-async (&optional n)
  "Run alternating updates of mermaid and code N times (default 7) asynchronously."
  (interactive)
  (make-process
   :name "llemacs-auto-run"
   :buffer llemacs--buf-main-pj
   :command (list "emacs"
                  "--batch"
                  "-L" (expand-file-name "~/.emacs.d/elpa/request-20230127.417")
                  "-L" (expand-file-name "~/.emacs.d/elpa/async-20241126.810")
                  "-L" (expand-file-name "~/.emacs.d/lisp/emacsql")
                  "-L" (expand-file-name "llemacs.el" llemacs--path)
                  "-L" (or load-file-name buffer-file-name)
                  "--eval"
                  (format "(progn
                           (load-file \"%s\")
                           (let ((num (or %s 7)))
                             (dotimes (i num)
                               (llemacs-pj-update-mermaid)
                               (llemacs-pj-step))))"
                          (locate-library "llemacs")
                          (or n 7)))))

(defun llemacs-run-auto (&optional n)
  "Run alternating updates of mermaid and code N times (default 7)."
  (interactive)
  (let ((num (or n 7)))
    (dotimes (i num)
      (llemacs-pj-update-mermaid)
      (llemacs-pj-step))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))