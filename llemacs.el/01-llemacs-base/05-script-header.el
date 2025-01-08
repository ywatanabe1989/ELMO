;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-07 10:25:07
;;; Timestamp: <2025-01-07 10:25:07>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/02-script-header.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


(defcustom llemacs--script-supported-types
  '(css py el sh yaml org md tex)
  "List of supported script types."
  :type '(repeat symbol)
  :group 'llemacs-script)

(defcustom llemacs--script-header-templates
  '((css . "/* Timestamp: \"%s (%s)\" */\n/* File: %s */\n\n")
    (py . "#!/usr/bin/env python3\n# -*- coding: utf-8 -*-\n# Timestamp: \"%s (%s)\"\n# File: %s\n\n__file__ = \"%s\"\n\n")
    (el . ";;; -*- coding: utf-8; lexical-binding: t -*-\n;;; Author: %s\n;;; Timestamp: <%s>\n;;; File: %s\n")
    (sh . "#!/bin/bash\n# Timestamp: \"%s (%s)\"\n# File: %s\n\nTHIS_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"\n\n")
    (yaml . "# Timestamp: \"%s (%s)\"\n# File: %s\n")
    (org . "# Timestamp: \"%s (%s)\"\n# File: %s\n\n")
    (md . "<!-- ---\n!-- Timestamp: %s\n!-- Author: %s\n!-- File: %s\n!-- --- -->\n")
    (tex . "%% Timestamp: \"%s (%s)\"\n%% File: %s\n\n"))
  "Templates for file headers."
  :type '(alist :key-type symbol :value-type string)
  :group 'llemacs-script)

(defcustom llemacs--script-header-patterns
  '((css . "\\`\\(?:/\\* Timestamp:.*\\*/\n\\)?\\(?:/\\* File:.*\\*/\n\\)?")
    (py . "\\`\\(?:#!.*\n\\)?\\(?:# -\\*-.*-\\*-\n\\)?\\(?:# Timestamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:\n\\)?\\(?:__file__.*\n\\)?\n?")
    (el . "\\`\\(?:;;;.*\n\\)\\{1,4\\}")
    (sh . "\\`\\(?:#!.*\n\\)?\\(?:# Timestamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:\n\\)?\\(?:THIS_DIR=.*\n\\)?\n?")
    (yaml . "\\`\\(?:# Timestamp:.*\n\\)?\\(?:# File:.*\n\\)?")
    (org . "\\`\\(?:# Timestamp:.*\n\\)?\\(?:# File:.*\n\\)?")
    (md . "\\`\\(?:<!-- ---\n\\)\\(?:!-- Timestamp:.*\n\\)\\(?:!-- Author:.*\n\\)\\(?:!-- File:.*\n\\)\\(?:!-- --- -->\n\\)")
    (tex . "\\`\\(?:%% Timestamp:.*\n\\)?\\(?:%% File:.*\n\\)?"))
  "Regular expression patterns to match file headers."
  :type '(alist :key-type symbol :value-type string)
  :group 'llemacs-script)

(defun llemacs--script-detect-type (file-path)
  "Detect script type from FILE-PATH."
  (let ((ext (file-name-extension file-path)))
    (and ext (intern ext))))

(defun llemacs--script-get-template (type)
  "Get header template for TYPE."
  (or (alist-get type llemacs--script-header-templates)
      (error "Unsupported script type: %s" type)))

(defun llemacs--script-get-pattern (type)
  "Get header pattern for TYPE."
  (or (alist-get type llemacs--script-header-patterns)
      (error "Unsupported script type: %s" type)))

(defun llemacs--script-format-header (type file-path)
  "Format header for TYPE and FILE-PATH."
  (let ((template (llemacs--script-get-template type))
        (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
        (user-name (user-login-name))
        (file-name (file-name-nondirectory file-path)))
    (pcase type
      ('py (format template time-string user-name file-name file-path))
      ('el (format template time-string time-string file-path))
      (_ (format template time-string user-name file-name)))))


(defun llemacs--script-update-header (file-path)
  "Update header in FILE-PATH."
  (when-let* ((type (llemacs--script-detect-type file-path))
              ((member type llemacs--script-supported-types)))
    (let* ((pattern (llemacs--script-get-pattern type))
           (header (cond
                    ((eq type 'md)
                     (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
                           (author user-login-name))
                       (format (alist-get type llemacs--script-header-templates)
                               timestamp author file-path)))
                    (t (llemacs--script-format-header type file-path)))))
      (save-excursion
        (goto-char (point-min))
        (if (looking-at pattern)
            (replace-match header)
          (insert header))))))

(defun llemacs--script-before-save-hook ()
  "Update script header before saving."
  (when (buffer-file-name)
    (llemacs--script-update-header (buffer-file-name))))

(dolist (mode '(python-mode-hook
                emacs-lisp-mode-hook
                sh-mode-hook
                yaml-ts-mode-hook
                yaml-mode-hook
                org-mode-hook
                markdown-mode-hook
                tex-mode-hook
                css-mode-hook))
  (add-hook mode
            (lambda ()
              (add-hook 'before-save-hook
                        'llemacs--script-before-save-hook nil t))))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
