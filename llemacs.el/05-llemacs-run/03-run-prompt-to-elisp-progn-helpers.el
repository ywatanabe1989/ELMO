;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 05:55:41
;;; Timestamp: <2025-01-09 05:55:41>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/03-run-prompt-to-elisp-progn-helpers.el

(require 'f)

(defvar org-latex-image-default-width "0.8\\linewidth"
  "Default width for LaTeX image exports.")

(defvar org-latex-pdf-process
  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to PDF.")

(defun llemacs--get-or-create-if-not-exists (path)
  "Create directories if needed and return PATH."
  (let ((dir (file-name-directory path)))
    (when dir
      (make-directory dir t))
    path))

(defun llemacs--path-pj-get-or-create-script-python (filename)
  "Get-Or-Create Python script path for FILENAME."
  (let ((path (llemacs--get-or-create-if-not-exists
               (expand-file-name filename (expand-file-name "python" llemacs--path-pj-scripts)))))
    path))

(defun llemacs--path-pj-get-or-create-script-elisp (filename)
  "Get-Or-Create Elisp script path for FILENAME."
  (let ((path (llemacs--get-or-create-if-not-exists
               (expand-file-name filename (expand-file-name "elisp" llemacs--path-pj-scripts)))))
    path))

(defun llemacs--path-pj-get-or-create-script-shell (filename)
  "Get-Or-Create Shell script path for FILENAME."
  (let ((path (llemacs--get-or-create-if-not-exists
               (expand-file-name filename (expand-file-name "shell" llemacs--path-pj-scripts)))))
    path))

(defun llemacs--path-pj-get-or-create-report-org (title)
  "Get-Or-Create org report path for TITLE."
  (llemacs--get-or-create-if-not-exists
   (expand-file-name (format "%s-%s-report.org" llemacs--timestamp title)
                     (expand-file-name (format "%s-%s" llemacs--timestamp title)
                                       llemacs--path-pj-reports))))

(defun llemacs--path-pj-get-or-create-report-pdf (title)
  "Get-Or-Create PDF report path for TITLE."
  (llemacs--get-or-create-if-not-exists
   (expand-file-name (format "%s-%s-report.pdf" llemacs--timestamp title)
                     (expand-file-name (format "%s-%s" llemacs--timestamp title)
                                       llemacs--path-pj-reports))))

(defun llemacs--path-pj-get-or-create-figure (filename title)
  "Get-Or-Create figure path for FILENAME with TITLE."
  (llemacs--get-or-create-if-not-exists
   (expand-file-name filename
                     (expand-file-name (format "%s-%s" llemacs--timestamp title)
                                       llemacs--path-pj-results-figures))))

(defun llemacs--path-pj-get-or-create-table (filename title)
  "Get-Or-Create table path for FILENAME with TITLE."
  (llemacs--get-or-create-if-not-exists
   (expand-file-name filename
                     (expand-file-name (format "%s-%s" llemacs--timestamp title)
                                       llemacs--path-pj-results-tables))))

(defun llemacs--path-pj-get-or-create-data (relative-file-path-from-the-data-dir)
  "Get-Or-Create path under data directory using RELATIVE-FILE-PATH-FROM-THE-DATA-DIR."
  (llemacs--get-or-create-if-not-exists
   (expand-file-name relative-file-path-from-the-data-dir
                     llemacs--path-pj-data)))

(defun llemacs--path-pj-cat-config (filename title)
  "Read config file FILENAME with TITLE."
  (llemacs--cat-file
   (llemacs--get-or-create-if-not-exists
    (expand-file-name filename llemacs--path-pj-config))))

(defun llemacs--path-pj-update-config (filename title)
  "Update config file FILENAME with TITLE."
  (llemacs--update-file
   (llemacs--get-or-create-if-not-exists
    (expand-file-name filename llemacs--path-pj-config))))

;; Org
(defun llemacs--org-write-standard-headers (title)
  "Write standard org headers with TITLE."
  (insert "#+STARTUP: showall\n")
  (insert "#+OPTIONS: toc:nil num:nil\n")
  (insert (format "#+TITLE: %s\n" title))
  (insert (format "#+DATE: %s\n\n" llemacs--timestamp))
  (insert (format "* Project Directory\n%s\n\n" llemacs--path-pj)))

(defun llemacs--org-write-figure (path &optional width)
  "Insert org figure with PATH and optional WIDTH."
  (insert (format "#+ATTR_HTML: :width %d\n" (or width 400)))
  (insert "#+ATTR_LATEX: :float t :placement [H]\n")
  (insert (format "[[file:%s]]\n\n" path)))

(defun llemacs--org-export-to-pdf (org-file pdf-file)
  "Export ORG-FILE to PDF-FILE."
  (let ((buf (find-file-noselect org-file)))
    (with-current-buffer buf
      (llemacs--logging-write-info-pj "Exporting org to PDF")
      (org-latex-export-to-pdf))
    buf))

(defun llemacs--org-setup-visualization (buf pdf-file)
  "Setup visualization in BUF and add PDF-FILE link."
  (with-current-buffer buf
    (when (file-exists-p pdf-file)
      (llemacs--logging-write-success-pj
       (format "PDF generated successfully: %s" pdf-file))
      (goto-char (point-max))
      (insert "\n* PDF\n")
      (insert (format "[[file:%s]]\n\n" pdf-file))
      (let ((buffer-save-without-query t))
        (save-buffer))
      (revert-buffer t t t)
      (org-display-inline-images))))

;; (defun llemacs--org-write-standard-headers (title)
;;   "Write standard org headers with TITLE."
;;   (insert (format "#+TITLE: LLEMACS Report\n"))
;;   (insert (format "#+DATE: %s\n\n" llemacs--timestamp))
;;   (insert (format "* Project Directory\n%s\n\n" llemacs--path-pj)))

;; (defun llemacs--org-write-figure (path &optional width)
;;   "Insert org figure with PATH and optional WIDTH."
;;   (insert (format "#+ATTR_HTML: :width %d\n" (or width 400)))
;;   (insert "#+ATTR_LATEX: :float t :placement [H]\n")
;;   (insert (format "[[file:%s]]\n\n" path)))

;; (defun llemacs--org-export-to-pdf (org-file pdf-file)
;;   "Export ORG-FILE to PDF-FILE."
;;   (let ((buf (find-file-noselect org-file)))
;;     (with-current-buffer buf
;;       (llemacs--logging-write-info-pj "Exporting org to PDF")
;;       (org-latex-export-to-pdf)
;;       pdf-file)))

;; (defun llemacs--org-setup-visualization (buf pdf-file)
;;   "Setup visualization in BUF and add PDF-FILE link."
;;   (with-current-buffer buf
;;     (when (file-exists-p pdf-file)
;;       (llemacs--logging-write-success-pj
;;        (format "PDF generated successfully: %s" pdf-file))
;;       (goto-char (point-max))
;;       (insert "\n* PDF\n")
;;       (insert (format "[[file:%s]]\n\n" pdf-file))
;;       (save-buffer)
;;       (revert-buffer t t)
;;       (org-inline-anim-mode 1)
;;       (org-display-inline-images)
;;       (let ((buffer-save-without-query t))
;;         (save-buffer))
;;       (revert-buffer t t))))

(defun llemacs--validate-paths (paths)
  "Validate existence of all PATHS. Return t if all exist, nil otherwise."
  (let ((valid t))
    (dolist (path paths valid)
      (unless (file-exists-p path)
        (error "File not found: %s" path)
        (setq valid nil)))))

(defun llemacs--org-write-figure (figure-path width)
  "Insert org-mode figure with FIGURE-PATH and WIDTH."
  (insert (format "#+ATTR_HTML: :width %dpx\n" width))
  (insert (format "[[file:%s]]\n\n" figure-path)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
