;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-01 14:33:31
;;; Time-stamp: <2025-01-01 14:33:31 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/06-llemacs-project-management/06-llemacs-step.el

;; (require '01-llemacs-config)
;; (require '02-llemacs-logging)
;; (require '02-llemacs-utils)
;; (require '04-llemacs-lang2elisp)
;; (require '06-llemacs-run)
;; (require '05-llemacs-project)
;; (require '06-llemacs-compress)

(defvar llemacs-step-context nil
  "Current context for LLEMACS step execution.")


(defun llemacs-step-execute (project-id prompt)
  "Execute single step with PROMPT in PROJECT-ID context."
  (let* ((context (llemacs-step-generate-context project-id))
         (elisp-code (llemacs-step-generate-elisp context))
         (result (llemacs-step-execute-elisp elisp-code project-id)))
    (llemacs-step-log-result context elisp-code result project-id)))

(defun llemacs-step-generate-context (project-id)
  "Generate context from PROJECT-ID workspace state."
  (llemacs-ensure-project-structure project-id)
  (let* ((project-dir (expand-file-name project-id llemacs-path-projects))
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
  (let* ((template-file (expand-file-name "000-context-to-elisp.md" llemacs--path-prompt-compiled))
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
  (let ((project-dir (expand-file-name project-id llemacs-path-projects)))
    (condition-case err
        (progn
          (cd project-dir)
          (llemacs-exec-local code))
      (error
       (llemacs--logging-log-error (format "Step execution failed in project %s: %s"
                                       project-id err))
       nil))))

(defun llemacs-step-log-result (context code result project-id)
  "Log step execution RESULT with CONTEXT and CODE for PROJECT-ID."
  (let* ((project-dir (expand-file-name project-id llemacs-path-projects))
         (steps-dir (expand-file-name "steps" project-dir))
         (step-file (expand-file-name
                     (format "step-%s.log"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     steps-dir))
         (log-entry `((timestamp . ,(current-time))
                      (context . ,context)
                      (code . ,code)
                      (result . ,result))))
    (llemacs--logging-to-file
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
                                         (expand-file-name project-id llemacs-path-projects))))
      (when (file-exists-p report)
        (find-file report)))
    log-entry))

(provide '05-llemacs-step)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))