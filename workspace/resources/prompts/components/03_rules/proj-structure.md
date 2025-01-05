<!-- ---
!-- title: 2025-01-05 08:32:57
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/proj-structure.md
!-- --- -->

# Rule: proj-structure
* The ID and name of the current project is pre-defined as `llemacs--cur-pj` (e.g., `000-sample-project`).
* Paths to directory and files for this project workspace is pre-defined as Elisp variables as shown below.
* For example, this is the default project structure:

000-sample-project (= `llemacs--path-pj` instead of `llemacs--path-pj-root`)
|-- config (= `llemacs--path-pj-config`)
|   `-- yaml
|-- data (= `llemacs--path-pj-data`)
|   `-- patient_01
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
|   |-- project_management.gif
|   |-- project_management.mmd (= `llemacs--path-pj-pm-mmd`)
|   |-- project_management.png
|   `-- project_management.svg
|-- README.md
|-- requirements.txt
|-- results (= `llemacs--path-pj-results`)
|   |-- figures
|   |   |-- gif
|   |   |-- jpg
|   |   `-- png
|   |-- reports
|   |   |-- org
|   |   `-- pdf
|   `-- tables
`-- scripts (= `llemacs--path-pj-scripts`)
    |-- elisp
    `-- python

* Edit project directory organization appropriately.
* Current project tree will be included as part of prompt, as context.
* For example, I am am fond of this kind of organization: