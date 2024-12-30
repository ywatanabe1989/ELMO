;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-29 08:11:27
;;; Time-stamp: <2024-12-29 08:11:27 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/13-llemacs-step.el

(require '01-llemacs-config)
(require '02-llemacs-logging)
(require '04-llemacs-utils)
(require '09-llemacs-lang2elisp)
(require '10-llemacs-run)
(require '11-llemacs-project)
(require '10-llemacs-compressor)

(defvar llemacs-step-context nil
  "Current context for LLEMACS step execution.")

(defun llemacs-step-generate-context (project-id)
  "Generate context from PROJECT-ID workspace state."
  (llemacs-ensure-project-structure project-id)
  (let* ((project-dir (expand-file-name project-id llemacs-projects-dir))
         (context-dir (expand-file-name "context" project-dir))
         (memory-file (expand-file-name "memory.json" project-dir))
         (status-file (expand-file-name "status.json" project-dir))
         (metadata-file (expand-file-name "metadata.json" project-dir))
         (context `((project . ,project-id)
                    (messages . ,(directory-files context-dir t "\\.md$"))
                    (memory . ,(when (file-exists-p memory-file)
                                 (llemacs-compress-json (json-read-file memory-file) 512)))
                    (status . ,(when (file-exists-p status-file)
                                 (json-read-file status-file)))
                    (metadata . ,(when (file-exists-p metadata-file)
                                   (json-read-file metadata-file))))))
    (setq llemacs-step-context context)
    context))

(defun llemacs-step-generate-elisp (context)
  "Generate Elisp code based on CONTEXT."
  (let* ((template-file (expand-file-name "000-context-to-elisp.md" llemacs-prompt-compiled-dir))
         (template (with-temp-buffer
                     (insert-file-contents template-file)
                     (buffer-string)))
         (compressed-context (llemacs-compress-json context 1024))
         (prompt (replace-regexp-in-string "PLACEHOLDER"
                                           (format "%S" compressed-context)
                                           template)))
    (llemacs-lang2elisp prompt)))

(defun llemacs-step-execute-elisp (code project-id)
  "Execute generated CODE safely within PROJECT-ID context."
  (let ((project-dir (expand-file-name project-id llemacs-projects-dir)))
    (condition-case err
        (progn
          (cd project-dir)
          (llemacs-exec-local code))
      (error
       (llemacs--log-error (format "Step execution failed in project %s: %s"
                                project-id err))
       nil))))

(defun llemacs-step-log-result (context code result project-id)
  "Log step execution RESULT with CONTEXT and CODE for PROJECT-ID."
  (let* ((project-dir (expand-file-name project-id llemacs-projects-dir))
         (steps-dir (expand-file-name "steps" project-dir))
         (step-file (expand-file-name
                     (format "step-%s.log"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     steps-dir))
         (log-entry `((timestamp . ,(current-time))
                      (context . ,context)
                      (code . ,code)
                      (result . ,result))))
    (llemacs--log-to-file
     (format "Step Result:\n%S" log-entry)
     step-file)
    log-entry))

(defun llemacs-step (project-id)
  "Execute one step of LLEMACS's think-act cycle for PROJECT-ID."
  (interactive "sProject ID: ")
  (let* ((context (llemacs-step-generate-context project-id))
         (code (llemacs-step-generate-elisp context))
         (result (llemacs-step-execute-elisp code project-id))
         (log-entry (llemacs-step-log-result context code result project-id)))
    (when-let ((report (expand-file-name "report.org"
                                         (expand-file-name project-id llemacs-projects-dir))))
      (when (file-exists-p report)
        (find-file report)))
    log-entry))

(provide '13-llemacs-step)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))