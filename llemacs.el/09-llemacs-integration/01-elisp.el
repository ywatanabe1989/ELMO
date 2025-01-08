;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 04:14:30
;;; Timestamp: <2025-01-09 04:14:30>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/01-elisp.el

;; Elisp integration
(defun llemacs--run-elisp-script (script &optional args)
  "Run Emacs Lisp SCRIPT with ARGS."
  (llemacs--logging-write-info-pj
   (format "Running Elisp script: %s %s" script args))
  (condition-case err
      (progn
        (load script)
        (when args
          (eval `(funcall ',(intern (file-name-base script)) ,@args)))
        (llemacs--logging-write-success-pj
         (format "Elisp script completed: %s" script)))
    (error
     (error
      (format "Elisp script failed: %s - %s" script err)))))
