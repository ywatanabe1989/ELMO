;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 01:22:43
;;; Time-stamp: <2025-01-06 01:22:43 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/000-list.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 14:41:10
;;; Time-stamp: <2025-01-04 14:41:10 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/000-list.el

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

(defun llemacs--list-groups (&optional pattern)
  "List all llemacs custom groups matching PATTERN."
  (let ((groups))
    (mapatoms
     (lambda (symbol)
       (when (and (string-match-p (or pattern "^llemacs-") (symbol-name symbol))
                  (get symbol 'custom-group))
         (push symbol groups))))
    (sort groups #'string-lessp)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))