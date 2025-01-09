<!-- Time-stamp: "2025-01-06 19:29:07 (ywatanabe)" -->
<!-- File: project-structure.md -->

# Example: project-structure

* Example project structure is as follows:
  ``` plaintext
  <project-id>-<project-name> (= `llemacs--path-pj` instead of `llemacs--path-pj-root`)
  |-- .env (= `llemacs--path-pj-python-env`)
  |   `-- bin
  |       |-- activate
  |       `-- python (= `llemacs--path-pj-python`)
  |-- config (= `llemacs--path-pj-config`)
  |   |-- <file_name>.yaml
  |   |-- <file_name>.yaml
  |   |...
  |   `-- <file_name>.yaml
  |-- data (= `llemacs--path-pj-data`)
  |   |-- <dir_name>
  |   |    |-- <file_name>.npy
  |   |    |-- <file_name>.txt
  |   |    |-- <file_name>.csv
  |   |    |-- <file_name>.h5
  |   |    |...
  |   |    `-- <file_name>.mat
  |   |-- <dir_name>
  |   |    |-- <file_name>.npy
  |   |    |-- <file_name>.txt
  |   |    |-- <file_name>.csv
  |   |    |-- <file_name>.h5
  |   |    |...
  |   |    `-- <file_name>.mat
  |   |...
  |   `-- <dir_name>
  |        |-- <file_name>.npy
  |        |-- <file_name>.txt
  |        |-- <file_name>.csv
  |        |-- <file_name>.h5
  |        |...
  |        `-- <file_name>.mat
  |-- logs (= `llemacs--path-pj-logs`)
  |   |-- by_level
  |   |   |-- debug.log   (= `llemacs--path-pj-logs-debug`, updated by `llemacs--logging-write-debug-pj`)
  |   |   |-- elisp.log   (= `llemacs--path-pj-logs-elisp`, updated by `llemacs--logging-write-elisp-pj`)
  |   |   |-- error.log   (= `llemacs--path-pj-logs-error`, updated by `llemacs--logging-write-error-pj`)
  |   |   |-- info.log    (= `llemacs--path-pj-logs-info`, updated by `llemacs--logging-write-info-pj`)
  |   |   |-- prompt.log  (= `llemacs--path-pj-logs-prompt`, updated by `llemacs--logging-write-prompt-pj`)
  |   |   |-- search.log  (= `llemacs--path-pj-logs-search`, updated by `llemacs--logging-write-search-pj`)
  |   |   |-- success.log (= `llemacs--path-pj-logs-success`, updated by `llemacs--logging-write-success-pj`)
  |   |   `-- warn.log    (= `llemacs--path-pj-logs-warn`, updated by `llemacs--logging-write-warn-pj`)
  |   `-- logging.log (= `llemacs--path-pj-logs-all`)
  |-- project_management
  |   |-- project_management.mmd (= `llemacs--path-pj-pm-mmd`)
  |   |-- project_management.png
  |   `-- project_management.svg
  |-- README.md
  |-- requirements.txt
  |-- results (= `llemacs--path-pj-results`)
  |   |-- figures
  |   |   |-- <file_name>.jpg
  |   |   |-- <file_name>.jpg
  |   |   |...
  |   |   `-- <file_name>.jpg
  |   `-- tables
  |       |-- <file_name>.csv
  |       |-- <file_name>.csv
  |       |...
  |       `-- <file_name>.csv
  |-- reports (= `llemacs--path-pj-reports`)
  |    |-- <timestamp>-<title>
  |    |   |-- <timestamp>-<title>-report.org
  |    |   `-- <timestamp>-<title>-report.pdf
  |    |-- <timestamp>-<title>
  |    |   |-- <timestamp>-<title>-report.org
  |    |   `-- <timestamp>-<title>-report.pdf
  |    |...
  |    `-- <timestamp>-<title>
  |        |-- <timestamp>-<title>-report.org
  |        `-- <timestamp>-<title>-report.pdf
  `-- scripts (= `llemacs--path-pj-scripts`)
       |-- elisp
       |     |-- <file_name>.el
       |     |-- <file_name>.el
       |     |...
       |     `-- <file_name>.el
       |-- shell
       |     |-- <file_name>.sh
       |     |-- <file_name>.sh
       |     |...
       |     `-- <file_name>.sh
       `-- python
             |-- <file_name>.py
             |-- <file_name>.py
             |...
             `-- <file_name>.py
  ```
