<!-- ---
!-- Timestamp: 2025-01-08 21:17:13
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/07_tools/code-elisp-progn-block.md
!-- --- -->

# Tool: code-elisp-progn-block

``` elisp
(defun llemacs--get-or-create-if-not-exists (path)
  "Create directories if needed and return PATH."
  ...)

(defun llemacs--path-pj-get-or-create-script-python (filename)
  "Get-Or-Create Python script path for FILENAME."
  ...)

(defun llemacs--path-pj-get-or-create-script-elisp (filename)
  "Get-Or-Create Elisp script path for FILENAME."
  ...)

(defun llemacs--path-pj-get-or-create-script-shell (filename)
  "Get-Or-Create Shell script path for FILENAME."
  ...)

(defun llemacs--path-pj-get-or-create-report-org (title)
  "Get-Or-Create org report path for TITLE."
  ...)

(defun llemacs--path-pj-get-or-create-report-pdf (title)
  "Get-Or-Create PDF report path for TITLE."
  ...)

(defun llemacs--path-pj-get-or-create-figure (filename title)
  "Get-Or-Create figure path for FILENAME with TITLE."
  ...)

(defun llemacs--path-pj-get-or-create-table (filename title)
  "Get-Or-Create table path for FILENAME with TITLE."
  ...)

(defun llemacs--path-pj-get-or-create-data (relative-file-path-from-the-data-dir)
  "Get-Or-Create path under data directory using RELATIVE-FILE-PATH-FROM-THE-DATA-DIR."
  ...)

(defun llemacs--path-pj-cat-config (filename title)
  "Read config file FILENAME with TITLE."
  ...)

(defun llemacs--path-pj-update-config (filename title)
  "Update config file FILENAME with TITLE."
  ...)

;; Org
(defun llemacs--org-write-standard-headers (title)
  "Write standard org headers with TITLE."
  ...)

(defun llemacs--org-write-figure (path &optional width)
  "Insert org figure with PATH and optional WIDTH."
  ...)

(defun llemacs--org-export-to-pdf (org-file pdf-file)
  "Export ORG-FILE to PDF-FILE."
  ...)

(defun llemacs--org-setup-visualization (buf pdf-file)
  "Setup visualization in BUF and add PDF-FILE link."
  ...)

(defun llemacs--validate-paths (paths)
  "Validate existence of all PATHS. Return t if all exist, nil otherwise."
  ...)

(defun llemacs--validate-script (script-path)
  "Validate script existence and contents at SCRIPT-PATH."
  ...)

(defun llemacs--script-supports-dry-run (script-path)
  "Check if script at SCRIPT-PATH supports dry-run option."
  ...)

(defun llemacs--run-with-validation (title script-path script-args &optional dry-run)
  "Run script with validation and optional DRY-RUN."
  ...)


(defun llemacs--org-write-figure (figure-path width)
  "Insert org-mode figure with FIGURE-PATH and WIDTH."
  ...)
```

