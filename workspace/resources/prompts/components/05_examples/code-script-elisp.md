<!-- ---
!-- Timestamp: 2025-01-09 18:08:19
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/examples/code-script-elisp.md
!-- --- -->

# Exmple: code-script-elisp

```elisp
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 17:21:03
;;; Time-stamp: <2025-01-06 17:21:03 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/002-script-header.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Core utility functions
(defun llemacs--compile-latex (tex-file)
  (llemacs--logging-write-info-pj
   (format "Compiling LaTeX file: %s" tex-file))
  (let ((pdflatex-bin (llemacs--path-find-bin "pdflatex"))
        (command (format "%s %s" pdflatex-bin tex-file)))
    (condition-case err
        (llemacs--run-shell-command command)
      (error
       (llemacs--logging-write-error-pj
        (format "LaTeX compilation failed: %s" err))))))

(defun llemacs--analyze-data (input output)
  (condition-case err
      (let ((python-bin (llemacs--path-find-bin "python" "python3")))
        (llemacs--run-python-script
         python-bin 
         "analyze.py"
         "--input" input
         "--output" output))
    (error
     (llemacs--logging-write-error-pj
      (format "Data analysis failed: %s" err)))))

(defun llemacs--commit-and-push (files message branch)
  (condition-case err
      (progn
        (dolist (file files)
          (llemacs--run-git-command "add" file))
        (llemacs--git-commit message)
        (llemacs--git-push branch))
    (error
     (llemacs--logging-write-error-pj
      (format "Git operations failed: %s" err)))))

(defun llemacs--create-ticket (title body)
  (condition-case err
      (llemacs--run-gh-command "issue" "create"
                              "--title" title
                              "--body" body)
    (error
     (llemacs--logging-write-error-pj
      (format "Failed to create ticket: %s" err)))))

;; Main workflow
(defun llemacs--run-workflow ()
  (llemacs--logging-write-debug-pj "Starting workflow")
  
  (let ((tex-file "document.tex")
        (input-data "data.csv")
        (output-data "results.json"))

    (llemacs--compile-latex tex-file)
    (llemacs--analyze-data input-data output-data)
    (llemacs--commit-and-push 
     (list tex-file output-data)
     "Update documentation and analysis" 
     "main")
    (llemacs--create-ticket 
     "Documentation Update" 
     "Latest analysis results have been committed"))
  
  (llemacs--logging-write-success-pj "Workflow completed successfully"))

;; Execute when loading
(when load-file-name
  (llemacs--run-workflow))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
```
