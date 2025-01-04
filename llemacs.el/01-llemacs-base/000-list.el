;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 22:01:12
;;; Time-stamp: <2025-01-03 22:01:12 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/001-list.el

(defun llemacs-list (type &optional pattern)
  "List llemacs symbols by TYPE with optional PATTERN."
  (interactive
   (list nil))
  (let* ((type (or type
                   (completing-read "Type (command/variable/function/group): "
                                    '("command" "variable" "function" "group")
                                    nil t)))
         (pat (or pattern "^llemacs-")))
    (cond
     ((equal type "command") (apropos-command pat))
     ((equal type "variable") (apropos-variable pat))
     ((equal type "function") (apropos-function pat))
     ((equal type "group") (llemacs--list-groups pat))
     (t (error "Invalid type: %s" type)))))

(defun llemacs--list-groups ()
  "List all llemacs custom groups."
  (interactive)
  (let ((groups))
    (mapatoms
     (lambda (symbol)
       (when (and (string-match "^llemacs-" (symbol-name symbol))
                  (get symbol 'custom-group))
         (push symbol groups))))
    (dolist (group (sort groups #'string-lessp))
      (princ (format "%s\n" group)))))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))