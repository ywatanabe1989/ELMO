;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-06 10:03:05
;;; Time-stamp: <2025-01-06 10:03:05 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/002-script-header.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(defcustom llemacs--script-header-py
  "#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: \"%s (%s)\"
# File: %s

__file__ = \"%s\"

# HEADER ENDS
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-el
  ";;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: %s
;;; Time-stamp: <%s>
;;; File: %s

;;; HEADER ENDS
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-sh
  "#!/bin/bash
# Time-stamp: \"%s (%s)\"
# File: %s

THIS_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"

# HEADER ENDS
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-yaml
  "# Time-stamp: \"%s (%s)\"
# File: %s

# HEADER ENDS
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-js
  "// Time-stamp: \"%s (%s)\"
// File: %s

// HEADER ENDS
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-css
  "/* Time-stamp: \"%s (%s)\" */
/* File: %s */

/* HEADER ENDS */
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-html
  "<!-- Time-stamp: \"%s (%s)\" -->
<!-- File: %s -->

<!-- HEADER ENDS -->
",
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-org
  "# #+TITLE: %s
# #+AUTHOR: %s
# #+DATE: %s

# HEADER ENDS
")

(defcustom llemacs--script-header-md
  "<!-- ---
!-- title: %s
!-- author: %s
!-- date: %s
!-- --- -->

<!-- ---
!-- HEADER ENDS
!-- --- -->
")

(defcustom llemacs--script-header-tex
  "%%%% Time-stamp: \"%s (%s)\"
%%%% File: %s

%%%% HEADER ENDS
")

(defcustom llemacs--script-header-ps1
  "# Time-stamp: \"%s (%s)\"
# File: %s

# HEADER ENDS
")

(defcustom llemacs--script-header-sql
  "-- Time-stamp: \"%s (%s)\"
-- File: %s

-- HEADER ENDS
")

(defcustom llemacs--script-header-expression-py
  "\\`\\(?:#!.*\n\\)?\\(?:# -\\*-.*-\\*-\n\\)?\\(?:# Time-stamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:__file__.*\n\\)?\\(?:# HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-el
  "\\`\\(?:;;;.*\n\\)\\{1,4\\}\\(?:;;; HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-sh
  "\\`\\(?:#!.*\n\\)?\\(?:# Time-stamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:THIS_DIR=.*\n\\)?\\(?:# HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-yaml
  "\\`\\(?:# Time-stamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:# HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-js
  "\\`\\(?:// Time-stamp:.*\n\\)?\\(?:// File:.*\n\\)?\\(?:// HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-css
  "\\`\\(?:/\\* Time-stamp:.*\\*/\n\\)?\\(?:/\\* File:.*\\*/\n\\)?\\(?:/\\* HEADER ENDS \\*/\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-html
  "\\`\\(?:<!-- Time-stamp:.*-->\n\\)?\\(?:<!-- File:.*-->\n\\)?\\(?:<!-- HEADER ENDS -->\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-org
  "\\`\\(?:# \\+TITLE:.*\n\\)?\\(?:# \\+AUTHOR:.*\n\\)?\\(?:# \\+DATE:.*\n\\)?\\(?:# HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-md
  "\\`\\(?:<!-- ---\n\\)?\\(?:!-- title:.*\n\\)?\\(?:!-- author:.*\n\\)?\\(?:!-- date:.*\n\\)?\\(?:!-- --- -->\n\\)?\\(?:<!-- ---\n\\)?\\(?:!-- HEADER ENDS\n\\)?\\(?:!-- --- -->\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-tex
  "\\`\\(?:%%%% Time-stamp:.*\n\\)?\\(?:%%%% File:.*\n\\)?\\(?:%%%% HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-ps1
  "\\`\\(?:# Time-stamp:.*\n\\)?\\(?:# File:.*\n\\)?\\(?:# HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defcustom llemacs--script-header-expression-sql
  "\\`\\(?:-- Time-stamp:.*\n\\)?\\(?:-- File:.*\n\\)?\\(?:-- HEADER ENDS\n\\)?"
  :type 'string
  :group 'llemacs-script
  :group 'llemacs-sys)

(defun llemacs--script-update-header-py (python-script-path)
  "Update Python script header at PYTHON-SCRIPT-PATH."
  (let* ((header llemacs--script-header-py)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory python-script-path))
         (full-path (expand-file-name python-script-path))
         (formatted-header (format header
                                   time-string
                                   user-name
                                   file-name
                                   full-path)))
    (with-temp-buffer
      (when (file-exists-p python-script-path)
        (insert-file-contents python-script-path))
      (if (looking-at llemacs--script-header-expression-py)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) python-script-path))))

(defun llemacs--script-update-header-el (elisp-path)
  "Update Emacs Lisp script header at ELISP-PATH."
  (let* ((header llemacs--script-header-el)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (expand-file-name elisp-path))
         (formatted-header (format header time-string time-string file-name)))
    (with-temp-buffer
      (when (file-exists-p elisp-path)
        (insert-file-contents elisp-path))
      (if (looking-at llemacs--script-header-expression-el)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) elisp-path))))

(defun llemacs--script-update-header-sh (sh-path)
  "Update Shell script header at SH-PATH."
  (let* ((header llemacs--script-header-sh)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory sh-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p sh-path)
        (insert-file-contents sh-path))
      (if (looking-at llemacs--script-header-expression-sh)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) sh-path))))


(defun llemacs--script-update-header-yaml (yaml-path)
  "Update YAML header at YAML-PATH."
  (let* ((header llemacs--script-header-yaml)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory yaml-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p yaml-path)
        (insert-file-contents yaml-path))
      (if (looking-at llemacs--script-header-expression-yaml)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) yaml-path))))

(defun llemacs--script-update-header-js (js-path)
  "Update JavaScript header at JS-PATH."
  (let* ((header llemacs--script-header-js)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory js-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p js-path)
        (insert-file-contents js-path))
      (if (looking-at llemacs--script-header-expression-js)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) js-path))))

(defun llemacs--script-update-header-css (css-path)
  "Update CSS header at CSS-PATH."
  (let* ((header llemacs--script-header-css)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory css-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p css-path)
        (insert-file-contents css-path))
      (if (looking-at llemacs--script-header-expression-css)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) css-path))))

(defun llemacs--script-update-header-html (html-path)
  "Update HTML header at HTML-PATH."
  (let* ((header llemacs--script-header-html)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory html-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p html-path)
        (insert-file-contents html-path))
      (if (looking-at llemacs--script-header-expression-html)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) html-path))))


(defun llemacs--script-update-header-org (org-path)
  "Update Org header at ORG-PATH."
  (let* ((header llemacs--script-header-org)
         (file-name (file-name-base org-path))
         (user-name (user-login-name))
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (formatted-header (format header file-name user-name time-string)))
    (with-temp-buffer
      (when (file-exists-p org-path)
        (insert-file-contents org-path))
      (if (looking-at llemacs--script-header-expression-org)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) org-path))))

(defun llemacs--script-update-header-md (md-path)
  "Update Markdown header at MD-PATH."
  (let* ((header llemacs--script-header-md)
         (file-name (file-name-base md-path))
         (user-name (user-login-name))
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (formatted-header (format header file-name user-name time-string)))
    (with-temp-buffer
      (when (file-exists-p md-path)
        (insert-file-contents md-path))
      (if (looking-at llemacs--script-header-expression-md)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) md-path))))

(defun llemacs--script-update-header-tex (tex-path)
  "Update TeX header at TEX-PATH."
  (let* ((header llemacs--script-header-tex)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory tex-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p tex-path)
        (insert-file-contents tex-path))
      (if (looking-at llemacs--script-header-expression-tex)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) tex-path))))

(defun llemacs--script-update-header-ps1 (ps1-path)
  "Update PowerShell header at PS1-PATH."
  (let* ((header llemacs--script-header-ps1)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory ps1-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p ps1-path)
        (insert-file-contents ps1-path))
      (if (looking-at llemacs--script-header-expression-ps1)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) ps1-path))))

(defun llemacs--script-update-header-sql (sql-path)
  "Update SQL header at SQL-PATH."
  (let* ((header llemacs--script-header-sql)
         (time-string (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user-name (user-login-name))
         (file-name (file-name-nondirectory sql-path))
         (formatted-header (format header time-string user-name file-name)))
    (with-temp-buffer
      (when (file-exists-p sql-path)
        (insert-file-contents sql-path))
      (if (looking-at llemacs--script-header-expression-sql)
          (replace-match formatted-header)
        (goto-char (point-min))
        (insert formatted-header))
      (write-region (point-min) (point-max) sql-path))))

(defun llemacs--script-validate-header-py (header-string)
  "Return t if HEADER-STRING matches Python header pattern."
  (string-match-p llemacs--script-header-expression-py header-string))

(defun llemacs--script-validate-header-el (header-string)
  "Return t if HEADER-STRING matches Emacs Lisp header pattern."
  (string-match-p llemacs--script-header-expression-el header-string))

(defun llemacs--script-validate-header-sh (header-string)
  "Return t if HEADER-STRING matches Shell script header pattern."
  (string-match-p llemacs--script-header-expression-sh header-string))

(defun llemacs--script-validate-header-yaml (header-string)
  "Return t if HEADER-STRING matches YAML header pattern."
  (string-match-p llemacs--script-header-expression-yaml header-string))

(defun llemacs--script-validate-header-js (header-string)
  "Return t if HEADER-STRING matches JavaScript header pattern."
  (string-match-p llemacs--script-header-expression-js header-string))

(defun llemacs--script-validate-header-css (header-string)
  "Return t if HEADER-STRING matches CSS header pattern."
  (string-match-p llemacs--script-header-expression-css header-string))

(defun llemacs--script-validate-header-html (header-string)
  "Return t if HEADER-STRING matches HTML header pattern."
  (string-match-p llemacs--script-header-expression-html header-string))

(defun llemacs--script-validate-header-org (header-string)
  "Return t if HEADER-STRING matches Org header pattern."
  (string-match-p llemacs--script-header-expression-org header-string))

(defun llemacs--script-validate-header-md (header-string)
  "Return t if HEADER-STRING matches Markdown header pattern."
  (string-match-p llemacs--script-header-expression-md header-string))

(defun llemacs--script-validate-header-tex (header-string)
  "Return t if HEADER-STRING matches TeX header pattern."
  (string-match-p llemacs--script-header-expression-tex header-string))

(defun llemacs--script-validate-header-ps1 (header-string)
  "Return t if HEADER-STRING matches PowerShell header pattern."
  (string-match-p llemacs--script-header-expression-ps1 header-string))

(defun llemacs--script-validate-header-sql (header-string)
  "Return t if HEADER-STRING matches SQL header pattern."
  (string-match-p llemacs--script-header-expression-sql header-string))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))