;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 22:20:54
;;; Timestamp: <2025-01-08 22:20:54>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/03-run-prompt-to-elisp-progn-helpers.el

(require 'f)

(defvar org-latex-image-default-width "0.8\\linewidth"
  "Default width for LaTeX image exports.")

(defvar org-latex-pdf-process
  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to PDF.")

;; (defvar org-latex-image-default-width "0.8\\linewidth"
;;   "Default width for LaTeX image exports.")

;; (defvar org-latex-pdf-process
;;   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;   "Commands to process a LaTeX file to PDF.")

(defun llemacs--get-or-create-if-not-exists (path)
  "Create directories if needed and return PATH."
  (let ((dir (file-name-directory path)))
    (when dir
      (make-directory dir t))
    path))

;; (defun llemacs--path-pj-get-or-create-script-python (filename)
;;   "Get-Or-Create Python script path for FILENAME."
;;   (llemacs--get-or-create-if-not-exists
;;    (expand-file-name filename (expand-file-name "python" llemacs--path-pj-scripts))))

;; (defun llemacs--path-pj-get-or-create-script-elisp (filename)
;;   "Get-Or-Create Elisp script path for FILENAME."
;;   (llemacs--get-or-create-if-not-exists
;;    (expand-file-name filename (expand-file-name "elisp" llemacs--path-pj-scripts))))

;; (defun llemacs--path-pj-get-or-create-script-shell (filename)
;;   "Get-Or-Create Shell script path for FILENAME."
;;   (llemacs--get-or-create-if-not-exists
;;    (expand-file-name filename (expand-file-name "shell" llemacs--path-pj-scripts))))

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
  (insert (format "#+TITLE: LLEMACS Report\n"))
  (insert (format "#+DATE: %s\n\n" llemacs--timestamp))
  (insert (format "* Project Directory\n%s\n\n" llemacs--path-pj)))

(defun llemacs--org-write-figure (path &optional width)
  "Insert org figure with PATH and optional WIDTH."
  (insert (format "#+ATTR_HTML: :width %d\n" (or width 400)))
  (insert "#+ATTR_LATEX: :float t :placement [H]\n")
  (insert (format "[[file:%s]]\n\n" path)))

;; (defun llemacs--org-export-to-pdf (org-file pdf-file)
;;   "Export ORG-FILE to PDF-FILE."
;;   (let ((buf (find-file-noselect org-file)))
;;     (with-current-buffer buf
;;       (let ((org-latex-pdf-process
;;              '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;             (org-latex-image-default-width "0.8\\linewidth"))
;;         (llemacs--logging-write-info-pj "Exporting org to PDF")
;;         (org-latex-export-to-pdf)))
;;     buf))

;; (defun llemacs--org-export-to-pdf (org-file pdf-file)
;;   "Export ORG-FILE to PDF-FILE."
;;   (let ((buf (find-file-noselect org-file)))
;;     (with-current-buffer buf
;;       (llemacs--logging-write-info-pj "Exporting org to PDF")
;;       (org-latex-export-to-pdf))
;;     buf))

(defun llemacs--org-export-to-pdf (org-file pdf-file)
  "Export ORG-FILE to PDF-FILE."
  (let ((buf (find-file-noselect org-file)))
    (with-current-buffer buf
      (llemacs--logging-write-info-pj "Exporting org to PDF")
      (org-latex-export-to-pdf)
      pdf-file)))

(defun llemacs--org-setup-visualization (buf pdf-file)
  "Setup visualization in BUF and add PDF-FILE link."
  (with-current-buffer buf
    (when (file-exists-p pdf-file)
      (llemacs--logging-write-success-pj
       (format "PDF generated successfully: %s" pdf-file))
      (goto-char (point-max))
      (insert "\n* PDF\n")
      (insert (format "[[file:%s]]\n\n" pdf-file))
      (save-buffer)
      (revert-buffer t t)
      (org-inline-anim-mode 1)
      (org-display-inline-images)
      (let ((buffer-save-without-query t))
        (save-buffer))
      (revert-buffer t t))))

(defun llemacs--validate-paths (paths)
  "Validate existence of all PATHS. Return t if all exist, nil otherwise."
  (let ((valid t))
    (dolist (path paths valid)
      (unless (file-exists-p path)
        (llemacs--logging-write-error-pj
         (format "File not found: %s" path))
        (setq valid nil)))))

;; (defun llemacs--validate-script (script-path)
;;   "Validate script existence and contents at SCRIPT-PATH."
;;   (and (file-exists-p script-path)
;;        (with-temp-buffer
;;          (insert-file-contents script-path)
;;          (goto-char (point-min))
;;          ;; Basic checks for required components
;;          (and (search-forward "argparse" nil t)
;;               (search-forward "if __name__ == '__main__'" nil t)))))

(defun llemacs--script-supports-dry-run (script-path)
  "Check if script at SCRIPT-PATH supports dry-run option."
  (with-temp-buffer
    (insert-file-contents script-path)
    (goto-char (point-min))
    (search-forward "--dry-run" nil t)))


;; (defun llemacs--run-with-validation (title script-path script-args &optional dry-run)
;;   "Run script with validation and optional DRY-RUN."
;;   (message "Debug: Entering run-with-validation")
;;   (message "Debug: script-path: %s" script-path)
;;   (message "Debug: script-args: %s" script-args)
;;   (message "Debug: dry-run: %s" dry-run)

;;   (when (file-exists-p script-path)
;;     (set-file-modes script-path #o755))

;;   (let ((validated (and (llemacs--validate-script script-path)
;;                         (llemacs--validate-paths (list script-path)))))
;;     (message "Debug: validation result: %s" validated)

;;     (if validated
;;         (progn
;;           (message "Debug: Validation passed, proceeding with execution")
;;           (llemacs--logging-write-info-pj
;;            (format "Running %s script: %s"
;;                    (if dry-run "dry-run of" "")
;;                    script-path))
;;           (llemacs--run-shell
;;            (format "%s %s %s %s"
;;                    llemacs--path-pj-python
;;                    script-path
;;                    script-args
;;                    (if (and dry-run (llemacs--script-supports-dry-run script-path))
;;                        "--dry-run"
;;                      ""))))
;;       (progn
;;         (message "Debug: Validation failed")
;;         (llemacs--logging-write-error-pj "Validation failed")))))

(defun llemacs--run-with-validation (title script-path script-args &optional dry-run)
  "Run script with validation and optional DRY-RUN."
  (message "Debug: Entering run-with-validation")
  (message "Debug: script-path: %s" script-path)
  (message "Debug: script-args: %s" script-args)
  (message "Debug: dry-run: %s" dry-run)
  (when (file-exists-p script-path)
    (set-file-modes script-path #o755))
  (llemacs--logging-write-info-pj
   (format "Running %s script: %s"
           (if dry-run "dry-run of" "")
           script-path))
  (llemacs--run-shell
   (format "%s %s %s %s"
           llemacs--path-pj-python
           script-path
           script-args
           (if (and dry-run (llemacs--script-supports-dry-run script-path))
               "--dry-run"
             ""))))

(defun llemacs--org-write-figure (figure-path width)
  "Insert org-mode figure with FIGURE-PATH and WIDTH."
  (insert (format "#+ATTR_HTML: :width %dpx\n" width))
  (insert (format "[[file:%s]]\n\n" figure-path)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
