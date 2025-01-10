;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:54:21
;;; Timestamp: <2025-01-11 08:54:21>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/07-doc-generation.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 01:04:57
;;; Timestamp: <2025-01-11 01:04:57>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/07-doc-generation.el

;; (llemacs--run-step)
;; (llemacs--update-docs)

(defun llemacs--escape-quotes (text)
  "Escape markdown special characters in file at PATH."
  (setq text (replace-regexp-in-string "\\(‘\\).*\\'" "\"" text nil nil 1))
  (setq text (replace-regexp-in-string "\\(‘\\).*\\'" "\"" text nil nil 1))
  (setq text (replace-regexp-in-string "\\(’\\).*\\'" "\"" text nil nil 1))
  (setq text (replace-regexp-in-string "\\(\"\\).*\\'" "\"" text nil nil 1))
  text)
;; (llemacs--escape-quotes "See ‘llemacs--path-logs-update-pj’")


(defun llemacs--escape-quotes-in-file (file)
  "Escape markdown special characters in file at FILE."
  (message "Before: %s" (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))
  (with-temp-file file
    (insert-file-contents file)
    (let* ((text (buffer-string))
           (replaced-text (llemacs--escape-quotes text)))
      (erase-buffer)
      (insert replaced-text)))
  (message "After: %s" (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))

;; (llemacs--escape-quotes-in-file  "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/04_tools/llemacs-variables.md")

(defun llemacs--update-docs-functions ()
  "Create markdown documentation for llemacs functions."
  (let ((func-file (expand-file-name "tools/llemacs-functions.md" llemacs--path-res-prompt-components)))
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
          (let ((sym-name (llemacs--escape-quotes (symbol-name (car match))))
                (args (llemacs--escape-quotes (format "%S" (cdr match))))
                (doc (llemacs--escape-quotes
                      (or (documentation-property (car match) 'function-documentation) ""))))
            (insert (format "%s: %s\n%s\n\n" sym-name args doc)))))
      (insert "```\n"))))

(defun llemacs--update-docs-variables ()
  "Create markdown documentation for llemacs variables."
  (let ((var-file (expand-file-name "tools/llemacs-variables.md" llemacs--path-res-prompt-components))
        (sensitive-patterns '("api-key" "token" "secret" "password" "credential")))
    (with-temp-file var-file
      (insert "# Tool: llemacs-variables\n```plaintext\n")
      (mapatoms
       (lambda (sym)
         (when (and (boundp sym)
                    (string-match "llemacs" (symbol-name sym)))
           (let* ((value (symbol-value sym))
                  (name (symbol-name sym))
                  (should-mask-p (and (stringp value)
                                      (cl-some (lambda (pattern)
                                                 (string-match-p pattern name))
                                               sensitive-patterns)))
                  (sym-name (llemacs--escape-quotes (symbol-name sym)))
                  (doc (llemacs--escape-quotes (or (documentation-property sym 'variable-documentation) "")))
                  (val-str (if should-mask-p
                               "[MASKED]"
                             (llemacs--escape-quotes (format "%S" (if (stringp value) value (format "%S" value)))))))
             (insert (format "%s\n%s\nValue: %s\n\n" sym-name doc val-str))))))
      (insert "```\n"))))

(defun llemacs--update-docs ()
  "Create markdown documentation for llemacs functions and variables."
  (interactive)
  (llemacs--update-docs-functions)
  (llemacs--update-docs-variables))

;; (llemacs--update-docs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
