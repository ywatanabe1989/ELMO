<!-- ---
!-- title: 2025-01-05 01:32:02
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/proj-structure.md
!-- --- -->

# Rule: proj-structure
* The ID and name of the current project is pre-defined as `llemacs--cur-pj` (e.g., `000-sample-project`).
* Paths to directory and files for this project workspace is pre-defined as Elisp variables as shown below.
* For example, this is the default project structure:

./workspace/projects/000-sample-project/
|-- config
|-- data
|   `-- dmd
|       `-- demo_eeg_signal.npy
|-- logs
|   |-- all.log
|   |-- by_level
|   |   |-- debug.log
|   |   |-- elisp.log
|   |   |-- error.log
|   |   |-- info.log
|   |   |-- prompt.log
|   |   |-- search.log
|   |   |-- success.log
|   |   `-- warn.log
|   `-- logging.log
|-- project_management
|   |-- project_management.gif
|   |-- project_management.mmd
|   |-- project_management.png
|   `-- project_management.svg
|-- README.md
|-- requirements.txt
|-- results
|   |-- dmd
|   |   `-- dmd_analysis_01.jpg
|   |   `-- dmd_analysis_02.jpg
|   |   `-- dmd_report_20250105-011622.org
|   `-- project_init_report.org
`-- scripts
    `-- dmd
        `-- dmd_analysis.py

* Edit project directory organization appropriately.
* Current project tree will be included as part of prompt, as context.
* For example, I am am fond of this kind of organization: