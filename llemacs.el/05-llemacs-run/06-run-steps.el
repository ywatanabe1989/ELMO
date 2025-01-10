;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 12:35:19
;;; Timestamp: <2025-01-10 12:35:19>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/06-run-steps.el


(defvar llemacs--run-n-steps 7
  "The number of steps for 'llemacs-run-steps`")

(defun llemacs--run-step ()
  "Process one step for the current project."
  (condition-case err
      (progn
        (llemacs--run-update-project-management)
        (llemacs--run-advance-project))
    (void-function
     (llemacs--logging-write-error-pj
      (format "Void function error in run-step: %S" err)))))

(defun llemacs-run-steps (&optional n-steps)
  "Process N-STEPS steps for the current project.
If N-STEPS is nil, default to `llemacs--run-n-steps' steps."
  (let ((steps (or n-steps llemacs--run-n-steps)))
    (condition-case err
        (dotimes (_ steps)
          (llemacs--run-step))
      (void-function
       (llemacs--logging-write-error-pj
        (format "Void function error in run-steps: %S" err))))))

(defun llemacs-run-steps-async (&optional n-steps)
  "Process N-STEPS steps for the current project asynchronously.
If N-STEPS is nil, default to `llemacs--run-n-steps' steps."
  (make-process
   :name "llemacs-steps"
   :buffer llemacs--buf-main-pj
   :sentinel (lambda (process event)
               (llemacs--logging-write-info-pj (format "Process %s: %s" process event)))
   :filter (lambda (proc output)
             (with-current-buffer llemacs--buf-main-pj
               (goto-char (point-max))
               (insert output)))
   :command (append
             (list "emacs" "--batch")
             (mapcan (lambda (dir)
                       (list "-L" (expand-file-name dir)))
                     (directory-files "~/.emacs.d/elpa" t "^[^.]"))
             (list
              "-L" (expand-file-name "~/.emacs.d/lisp/emacsql")
              "-L" (expand-file-name "llemacs.el" llemacs--path)
              "-L" (or load-file-name buffer-file-name)
              "--eval"
              (format "(progn
                       (setq debug-on-error t)
                       (condition-case err
                           (progn
                             (load-file \"%s\")
                             (llemacs--pj-set-cur-pj \"%s\")
                             (let ((steps (or %s 7)))
                               (llemacs-run-steps steps)))
                         (error
                          (princ (format \"Error: %%S\\n\" err)))))"
                      (locate-library "llemacs")
                      llemacs--cur-pj
                      (or n-steps llemacs--run-n-steps))))))

;; (llemacs-run-steps 3)
;; (llemacs-run-steps-async 3)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
