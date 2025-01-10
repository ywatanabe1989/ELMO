;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-10 11:09:42
;;; Timestamp: <2025-01-10 11:09:42>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/07-documentation.el


(defun llemacs--update-docs ()
  "Create markdown documentation for llemacs functions and variables."
  (interactive)
  (let ((func-file (expand-file-name "tools/llemacs-functions.md" llemacs--path-res-prompt-components))
        (var-file (expand-file-name "tools/llemacs-variables.md" llemacs--path-res-prompt-components)))

    ;; Create functions documentation
    (with-temp-file func-file
      (insert "# Tool: llemacs-functions\n```plaintext\n")
      (let ((matches nil))
        (mapatoms
         (lambda (sym)
           (when (and (fboundp sym)
                      (string-match "llemacs" (symbol-name sym)))
             (push (cons sym (help-function-arglist sym)) matches))))
        (dolist (match (sort matches (lambda (a b)
                                       (string< (symbol-name (car a))
                                                (symbol-name (car b))))))
          (insert (format "%s: %S\n" (car match) (cdr match)))))
      (insert "```\n"))

    ;; Create variables documentation
    (with-temp-file var-file
      (insert "# Tool: llemacs-variables\n```plaintext\n")
      (mapatoms
       (lambda (sym)
         (when (and (boundp sym)
                    (string-match "llemacs" (symbol-name sym)))
           (insert (format "%s\n%s\nValue: %S\n\n"
                           sym
                           (or (documentation-property sym 'variable-documentation) "")
                           (symbol-value sym))))))
      (insert "```\n"))))
