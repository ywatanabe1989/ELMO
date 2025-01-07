;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 20:07:27
;;; Time-stamp: <2025-01-06 20:07:27 (ywatanabe)>
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
     (llemacs--logging-write-error-pj
      (format "Elisp script failed: %s - %s" script err)))))
