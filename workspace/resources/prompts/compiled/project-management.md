ROLES
====================
# Role: project-manager
* You are the project manager of our mutli-agent system on Emacs (Llemacs: LLM on Emacs).
--------------------
====================
TASKS
====================
# Task: project-management-understanding
* Understand the project and the hierarchy in our system: goals -> milestones -> tasks.
* Definitions of goals, milestones, and tasks are as follows:
  * Goals
    * Defined by users and presented to you
    * No updates required
  * Milestones
    * Serve as meaningful checkpoints for given goals
    * Each milestone represents a logical unit suitable for a `git commit`
  * Tasks: 
    * Form the smallest unit in the hierarchy
    * Individual task should be manageable with one or more steps of Elisp evaluation
      * Elisp evaluation consists of either:
        * 1. Loading one Elisp file
        * 2. Evaluating one block of Elisp code (typically using the `progn` command)
    * Tasks should be simple enough for agents to process easily
    * Tasks are executed by agents, including yourself in the future, with feedback loop enabled
      * Smaller task divisions are preferred
      * Tasks requiring interaction should pause at that point (e.g., when check the project structure using the `tree` command)
--------------------
# Task: project-management-review
* Review the project flow to accomplish the project goals
--------------------
# Task: project-management-revision
* Revise and update the entire workflow, milestones and tasks for the project
* Carefully review tags in the mermaid file (`todo`, `inProgress`, and `done`). 
  * This is critical as this file is the central communication tool between agents and users. 
    * `done` tag is only assigned when the task is certainly completed; `done` tasks will be not focused for other agents.
    * The worst case is where the `done` tag is applied but the task is not completed. This will cause other agents to skip such tasks. So, please carefully scrutinize progress tags.
--------------------
====================
RULES
====================
# Rule: code-elisp-progn-block
* An ELISP PROGN BLOCK must:
  * Be wrapped in a triple-quoted block with the pattern: ```elisp\n(progn\nYOUR CODE\n)\n```
  * Be self-contained, complete, and independently executable
  * Allow integration with external tools such as shell commands, Python, LaTeX, and other utilities
--------------------
# Rule: code-logging
* For logging, use the following custom Elisp functions:
  `(llemacs--logging-write-debug-pj message)`
  `(llemacs--logging-write-info-pj message)`
  `(llemacs--logging-write-success-pj message)`
  `(llemacs--logging-write-search-pj message)`
  `(llemacs--logging-write-elisp-pj message)`
  `(llemacs--logging-write-prompt-pj message)`
  `(llemacs--logging-write-api-pj message)`
  `(llemacs--logging-write-warn-pj message)`
  `(llemacs--logging-write-error-pj message)`
* Example:
  * `(llemacs--logging-write-error-pj "Variable X is not defined.")`
--------------------
# Rule: project-management-using-mermaid
* Project management is SOLELY handled by the mermaid file for the project.
* Breakdown complex tasks into small tasks. 
* Organize the progress and plans for the project into the mermaid file: `llemacs--path-pj-pm-mmd` (this file has the `.mmd` extension).
  * In the mermaid file, use state tracking tags (i.g., `todo`, `inProgress`, and `done`)
* The mermaid file is not for how to handle mermaid file but for the project itself.
* The mermaid file should include:
  * Project Description
  * Project Work Flow
  * Project Milestones
  * Project Tasks
  * The progress tags to visually indicate the project status
* The mermaid file should NOT include:
  * Project Directory
  * Legend
--------------------
====================
EXAMPLES
====================
# Exmple Output: project-management-mermaid

``` elisp
(progn
(let ((content "
graph TD
    subgraph Project Description
        PROJ[\"Project:
        000-MNIST-classification\"]:::done
        GOAL[\"Goal:
        Develop CNN model achieving >98% accuracy\"]:::done
        STATUS[\"Status:
        Model Training Phase\"]:::inProgress
    end

    subgraph MS1[\" \"]
        MILESTONE1[\"Milestone 1:
        Data Preparation\"]:::done
        MS1-Task1[Download MNIST]:::done
        MS1-Task2[Preprocess Data]:::done
        MS1-Task3[Create DataLoader]:::done
    end

    subgraph MS2[\" \"]
        MILESTONE2[\"Milestone 2:
        Model Development\"]:::inProgress
        MS2-Task1[CNN Architecture]:::done
        MS2-Task2[Training Loop]:::inProgress
        MS2-Task3[Hyperparameter Tuning]:::todo
    end

    subgraph MS3[\" \"]
        MILESTONE3[\"Milestone 3:
        Evaluation\"]:::todo
        MS3-Task1[Validation Metrics]:::todo
        MS3-Task2[Test Set Evaluation]:::todo
        MS3-Task3[Performance Analysis]:::todo
    end

    MILESTONE1 --> MS1-Task1 --> MS1-Task2 --> MS1-Task3
    MILESTONE2 --> MS2-Task1 --> MS2-Task2 --> MS2-Task3
    MILESTONE3 --> MS3-Task1 --> MS3-Task2 --> MS3-Task3

    classDef title fill:#e6e6e6,stroke:#333,stroke-width:2px,font-size:16px
    classDef starttag fill:#cce5ff,stroke:#333,stroke-width:2px
    classDef done fill:#9f9,stroke:#333,stroke-width:2px
    classDef inProgress fill:#ff9,stroke:#333,stroke-width:2px
    classDef todo fill:#fff,stroke:#333,stroke-width:2px
    classDef directory fill:#efe,stroke:#333,stroke-width:1px
    classDef endtag fill:#fcc,stroke:#333,stroke-width:2px
"))
(write-region content nil llemacs--path-pj-pm-mmd)
(llemacs--pj-mermaid-compile)))
```
--------------------
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
--------------------
====================
TOOLS
====================
# Tool: project-management-mermaid
To generate images from the mermaid file for the project, use this function.
``` elisp
(defun llemacs--pj-mermaid-compile ()
  "Compiles project management mermaid diagram into image files.
  Processes /project_management/project_management.mmd in current project
  ('llemacs--path-pj-pm-mmd'), generating corresponding
  PNG, SVG, and GIF files."
```
--------------------
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

(defun llemacs--org-write-figure (figure-path width)
  "Insert org-mode figure with FIGURE-PATH and WIDTH."
  ...)
  
(defun llemacs--logging-write-error-pj (message &optional project-id)
  "Log ERROR MESSAGE with PROJECT-ID."
  ...)

(defun llemacs--path-find-bin (name &rest alternatives)
  "Find executable NAME or its ALTERNATIVES and return path.
If none found, signal an error."
  ...)
```
--------------------
# Tool: llemacs-functions
```plaintext
llemacs: nil
llemacs--buf-disp: (buffer &optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-debug-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-debug-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-logging-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-logging-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-main-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-main-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-prompt-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-prompt-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-search-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-search-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-update-pj: nil
llemacs--check-json: (json-path)
llemacs--commit-and-push: (files message branch)
llemacs--create-ticket: (title body)
llemacs--def-buf-disp: (name sys-var pj-var)
llemacs--ensure-elisp-code: (code)
llemacs--ensure-gh-auth: nil
llemacs--extract-elisp-blocks: (text)
llemacs--extract-elisp-blocks-as-code: (text)
llemacs--format-log-entry: (log-entry)
llemacs--get-or-create-if-not-exists: (path)
llemacs--gh-create-issue: (bin title body)
llemacs--gh-create-pr: (bin title body base)
llemacs--git-add: (bin files)
llemacs--git-add-and-commit: (dir message)
llemacs--git-commit: (bin message)
llemacs--git-configure: nil
llemacs--git-ensure-branch: (bin branch)
llemacs--git-ensure-repo: (dir)
llemacs--git-init: (dir)
llemacs--git-log: (dir &optional limit)
llemacs--git-protect-main: (dir)
llemacs--git-push: (bin branch)
llemacs--git-reset: (dir &optional n-commits)
llemacs--git-resolve-conflicts: (dir)
llemacs--git-setup-gitignore: (dir)
llemacs--git-track: (dir files)
llemacs--git-unstage: (dir &optional files)
llemacs--git-untrack: (dir files)
llemacs--git-with-recipe-config: (dir recipe-name func &rest args)
llemacs--github-login: nil
llemacs--list-groups: (&optional pattern)
llemacs--llm-api-call-wrapper: (provider func &rest args)
llemacs--llm-api-check-rate-limit: (provider)
llemacs--llm-api-handle-error: (err provider)
llemacs--llm-claude: (text)
llemacs--llm-deepseek: (text)
llemacs--llm-gemini: (text)
llemacs--llm-prompt-compile: (recipe-id)
llemacs--llm-prompt-embed: (prompt recipe-id)
llemacs--llm-prompt-ensure-markdown-files: nil
llemacs--llm-prompt-get-available-recipe-ids: nil
llemacs--llm-prompt-get-recipe: (recipe-id)
llemacs--llm-prompt-get-template: (prompt-template-name)
llemacs--llm-prompt-get-templates: (&rest prompt-template-names)
llemacs--llm-prompt-open-templates: nil
llemacs--llm-prompt-write-compiled: (recipe-id content)
llemacs--llm-prompt2elisp: (prompt &optional recipe-id)
llemacs--llm-switch-provider: (provider)
llemacs--load-all-recipes: nil
llemacs--load-base-components: nil
llemacs--load-components: nil
llemacs--load-integration-components: nil
llemacs--load-json-file: (json-path)
llemacs--load-llm-components: nil
llemacs--load-logging-components: nil
llemacs--load-markdown-file: (file-path)
llemacs--load-project-management-components: nil
llemacs--load-recipe-file: (file)
llemacs--load-run-components: nil
llemacs--load-yaml-file: (file)
llemacs--logging-auto-refresh-mode: nil
llemacs--logging-backup-files: nil
llemacs--logging-define-loggers-sys: nil
llemacs--logging-ensure-log-file: (file-path)
llemacs--logging-format-message: (level message &optional full-project-name)
llemacs--logging-get-caller-info: nil
llemacs--logging-get-level-value: (level)
llemacs--logging-get-log-entries: (file-path)
llemacs--logging-get-logs: (&optional level is-pj)
llemacs--logging-get-logs-pj: (&optional level)
llemacs--logging-get-logs-sys: (&optional level)
llemacs--logging-meets-threshold-p: (level)
llemacs--logging-refresh: nil
llemacs--logging-refresh-buffer: nil
llemacs--logging-refresh-files: nil
llemacs--logging-should-log-p: (level)
llemacs--logging-view: (&optional select-level is-pj)
llemacs--logging-write: (level message &optional project-id enable-display)
llemacs--logging-write-api-pj: (message &optional project-id)
llemacs--logging-write-debug-pj: (message &optional project-id)
llemacs--logging-write-elisp-pj: (message &optional project-id)
llemacs--logging-write-error-pj: (string &rest args)
llemacs--logging-write-info-pj: (message &optional project-id)
llemacs--logging-write-prompt-pj: (message &optional project-id)
llemacs--logging-write-quiet: (content file-path &optional append)
llemacs--logging-write-search-pj: (message &optional project-id)
llemacs--logging-write-success-pj: (message &optional project-id)
llemacs--logging-write-warn-pj: (message &optional project-id)
llemacs--mermaid-compile: (&optional filename buffer)
llemacs--org-export-to-pdf: (org-file pdf-file)
llemacs--org-setup-visualization: (buf pdf-file)
llemacs--org-write-figure: (figure-path width)
llemacs--org-write-standard-headers: (title)
llemacs--path-create-log-paths-sys: nil
llemacs--path-define-log-paths-pj: nil
llemacs--path-find-bin: (name &rest alternatives)
llemacs--path-logs-all-pj: t
llemacs--path-logs-backup-pj: t
llemacs--path-logs-by-level-pj: t
llemacs--path-logs-pj: t
llemacs--path-logs-update-pj: nil
llemacs--path-pj-cat-config: (filename title)
llemacs--path-pj-get-or-create-data: (relative-file-path-from-the-data-dir)
llemacs--path-pj-get-or-create-figure: (filename title)
llemacs--path-pj-get-or-create-report-org: (title)
llemacs--path-pj-get-or-create-report-pdf: (title)
llemacs--path-pj-get-or-create-script-elisp: (filename)
llemacs--path-pj-get-or-create-script-python: (filename)
llemacs--path-pj-get-or-create-script-shell: (filename)
llemacs--path-pj-get-or-create-table: (filename title)
llemacs--path-pj-update: nil
llemacs--path-pj-update-config: (filename title)
llemacs--pj-auto-set: nil
llemacs--pj-buf-debug: t
llemacs--pj-buf-logging: t
llemacs--pj-buf-main: t
llemacs--pj-buf-prompt: t
llemacs--pj-buf-search: t
llemacs--pj-buf-update: nil
llemacs--pj-collect-context: (full-pj-name &optional log-level)
llemacs--pj-get-available-pjs: nil
llemacs--pj-get-contents: (path)
llemacs--pj-get-contents-log: (full-pj-name &optional log-level)
llemacs--pj-get-contents-pm-mmd: nil
llemacs--pj-get-contents-tree: (full-pj-name)
llemacs--pj-get-cur-pj: nil
llemacs--pj-get-default-pj: nil
llemacs--pj-get-dir: (full-pj-name)
llemacs--pj-get-last-pj: nil
llemacs--pj-get-latest-pj: nil
llemacs--pj-get-next-id: nil
llemacs--pj-get-path-log: (full-pj-name &optional log-level)
llemacs--pj-init: (pj-name &optional goals)
llemacs--pj-init-default: nil
llemacs--pj-init-pm-mmd: (full-pj-name pj-goals)
llemacs--pj-lock-acquire: (pj-id)
llemacs--pj-lock-check: (pj-id)
llemacs--pj-lock-check-stale: (pj-id)
llemacs--pj-lock-cleanup: nil
llemacs--pj-lock-cleanup-all: nil
llemacs--pj-lock-force-release: (pj-id &optional force)
llemacs--pj-lock-path: (pj-id)
llemacs--pj-lock-release: (pj-id)
llemacs--pj-mermaid-compile: nil
llemacs--pj-set-cur-pj: (pj-id &optional force)
llemacs--pj-set-last-pj: nil
llemacs--pj-switch: (pj-id &optional force)
llemacs--run-advance-project: nil
llemacs--run-elisp: (elisp-code)
llemacs--run-elisp-local: (elisp-code)
llemacs--run-elisp-script: (script &optional args)
llemacs--run-elisp-server: (elisp-code &optional emacs-server-file)
llemacs--run-gh-command: (bin &rest args)
llemacs--run-git-command: (bin &rest args)
llemacs--run-progn: (prompt)
llemacs--run-prompt: (prompt &optional recipe-id)
llemacs--run-prompt-after-run-hook-error: (llemacs--logging-write-error-pj prompt-text)
llemacs--run-prompt-after-run-hook-success: (elisp-code prompt-text)
llemacs--run-shell-command: (command)
llemacs--run-step: nil
llemacs--run-update-project-management: nil
llemacs--safe-read-file: (path)
llemacs--sanitize-content: (content)
llemacs--sanitize-filepath: (path)
llemacs--script-before-save-hook: nil
llemacs--script-detect-type: (file-path)
llemacs--script-format-header: (type file-path)
llemacs--script-get-pattern: (type)
llemacs--script-get-template: (type)
llemacs--script-update-header: (file-path)
llemacs--timestamp-get: nil
llemacs--timestamp-set: nil
llemacs--update-docs: nil
llemacs--validate-paths: (paths)
llemacs--validate-pj-id: (pj-id)
llemacs-list: (type &optional pattern)
llemacs-llm: (prompt &optional recipe-id)
llemacs-pj-step: nil
llemacs-pj-switch: (pj-id &optional force)
llemacs-pj-update-mermaid: nil
llemacs-run-auto: (&optional n)
llemacs-run-auto-async: (&optional n)
llemacs-run-steps: (&optional n-steps)
llemacs-run-steps-async: (&optional n-steps)
llemacs-timestamp: nil
my/tab-llemacs: nil
```
--------------------
# Tool: llemacs-variables
```plaintext
llemacs--logging-level-threshold
Minimum log level to record.
Value: info

llemacs--llm-api-key-anthropic
API key for Anthropic Claude.
Value: "[MASKED]"

llemacs--path-logs-api-pj
Log file paths for api for current project. (= ‘llemacs--path-pj-logs-api’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/api.log"

llemacs--path-python-env-sys
Path to the Python environment used by llemacs.el.
Value: "/home/ywatanabe/proj/llemacs/workspace/.env"

llemacs--path-res-scripts
Directory for LLEMACS scripts.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/scripts"

llemacs--run-n-steps
The number of steps for ’llemacs-run-steps‘
Value: 7

llemacs--path-res-prompts
Directory for LLEMACS templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts"

llemacs--path-pj-pm-mmd
Project management directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/project_management/project_management.mmd"

llemacs--buf-prompt-sys
Buffer for prompt operations.
Value: "*LLEMACS-PROMPT*"

llemacs--path-logs-info-sys
Path to LLEMACS system Information level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/info.log"

llemacs--path-logs-api-sys
Path to LLEMACS system API interaction logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/api.log"

llemacs--path-logs-all-sys
File path for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/all"

llemacs--path-latest-project-id
File to store the latest project ID.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/.project-id"

llemacs--llm-engine-deepseek
Model for DeepSeek.
Value: "deepseek-chat"

llemacs--path-logs-search-sys
Path to LLEMACS system Search operation logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/search.log"

llemacs--project-splitter
Splitter between logs.
Value: "
----------------------------------------
"

llemacs--path-logs-error-sys
Path to LLEMACS system Error level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/error.log"

llemacs--path-pj-logs-backup
Directory for current project logs backup.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/backup"

llemacs--path-pj-python
Python binary path for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/.env/bin/python"

llemacs--llm-api-timeout
Default timeout for API requests in seconds.
Value: 30

llemacs--logging-splitter
Splitter between logs.
Value: "
----------------------------------------
"

llemacs--log-levels-pj
Project-level logging-levels
Value: ((debug 0 . "Debug level logs") (info 1 . "Information level logs") (success 1 . "Success level logs") (prompt 1 . "Prompt operation logs") (elisp 1 . "Elisp execution logs") (api 1 . "API interaction logs") (search 1 . "Search operation logs") (warn 2 . "Warning level logs") (error 3 . "Error level logs"))

llemacs--path-pj-config
Configuration directory for the current project configuration.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/config"

llemacs--path-pj-root
Root directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects"

llemacs--path-pj-logs-api
Log file paths for api for current project. (= ‘llemacs--path-logs-api-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/api.log"

llemacs--path-pj-logs-all
File for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/all.log"

llemacs--path-agents
Directory for LLEMACS agents.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents"

llemacs--path-res-prompt-compiled
Directory for compiled prompt templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/compiled"

llemacs--path-res
Directory for LLEMACS resources.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources"

llemacs--path-res-prompt-components
Directory for prompt template components.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/components"

llemacs--path-agent
User-specific home directory for LLEMACS.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents/ywatanabe"

llemacs--path-res-secrets
Directory for storing sensitive information.
Value: "[MASKED]"

llemacs-project-auto-save
Whether to automatically save project state.
Value: t

llemacs--path-logs-prompt-sys
Path to LLEMACS system Prompt operation logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/prompt.log"

llemacs--logging-enable-display-threshold
Threshold level for displaying log messages. Logs at this level and above will be displayed.
Value: error

llemacs--llm-api-call-timestamps
Hash table tracking API call timestamps per provider.
Value: #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ())

llemacs-git-auto-add
Whether to automatically stage modified files.
Value: t

llemacs--path-pj-logs-info
Log file paths for info for current project. (= ‘llemacs--path-logs-info-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/info.log"

llemacs--path-logs-search-pj
Log file paths for search for current project. (= ‘llemacs--path-pj-logs-search’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/search.log"

llemacs--path-pj-logs
Directory for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs"

llemacs--path-pj-lock
Lock file for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/.lock"

llemacs--script-supported-types
List of supported script types.
Value: (css py el sh yaml org md tex)

llemacs--path-workspace
Base directory for LLEMACS workspace.
Value: "/home/ywatanabe/proj/llemacs/workspace"

llemacs--path-pj-logs-prompt
Log file paths for prompt for current project. (= ‘llemacs--path-logs-prompt-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/prompt.log"

llemacs--llm-engine-anthropic
Model for Anthropic Claude.
Value: "claude-3-5-sonnet-20241022"

llemacs--path-logs-success-sys
Path to LLEMACS system Success level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/success.log"

llemacs--path-pj-logs-search
Log file paths for search for current project. (= ‘llemacs--path-logs-search-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/search.log"

llemacs--buf-search-pj
Buffer for search results.
Value: "*LLEMACS-SEARCH ()*"

llemacs--cur-pj
Currently active project ID in the form of <id>-<project-name>.
Value: ""

llemacs--path-logs-info-pj
Log file paths for info for current project. (= ‘llemacs--path-pj-logs-info’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/info.log"

llemacs--buf-debug-pj
Buffer for debug output when debug mode enabled.
Value: "*LLEMACS-DEBUG ()*"

llemacs--buf-search-sys
Buffer for search results.
Value: "*LLEMACS-SEARCH*"

llemacs--llm-prompt-available-recipe-ids
List of available prompt-recipe IDs.
Value: nil

llemacs--path-logs-warn-sys
Path to LLEMACS system Warning level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/warn.log"

llemacs--path-logs-elisp-sys
Path to LLEMACS system Elisp execution logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/elisp.log"

llemacs--llm-api-endpoints
API endpoints for different LLM providers.
Value: ((openai . "https://api.openai.com/v1") (anthropic . "https://api.anthropic.com") (cohere . "https://api.cohere.ai/v1"))

llemacs--path-res-templates
Directory for LLEMACS templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/templates"

llemacs--llm-api-key-deepseek
API key for DeepSeek.
Value: "[MASKED]"

llemacs--path-agent-emacs-server
Path to LLEMACS Emacs server socket.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents/ywatanabe/.emacs.d/emacs-server/server"

llemacs--timestamp
Timestamp references by Llemacs.
Value: "2025-0110-221244"

llemacs--path-logs-prompt-pj
Log file paths for prompt for current project. (= ‘llemacs--path-pj-logs-prompt’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/prompt.log"

llemacs--path-pj-scripts
Scripts directory for the current project scripts.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/scripts"

llemacs--project-status-options
Available status options for LLEMACS projects.
Value: ("planning" "in-progress" "review" "completed" "archived")

llemacs--tab-counter
Counter for LLEMACS tab numbering.
Value: 0

llemacs--logging-max-size
Maximum size of log files in bytes (10MB default).
Value: 10485760

llemacs--path-pj-data
Data directory for the current project data.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/data"

llemacs--path-python-sys
Path to the Python binary used by llemacs.el.
Value: "/home/ywatanabe/proj/llemacs/workspace/.env/bin/python"

llemacs--path-pj-logs-elisp
Log file paths for elisp for current project. (= ‘llemacs--path-logs-elisp-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/elisp.log"

llemacs--llm-engine-google
Model for Google Claude.
Value: "gemini-2.0-flash-exp"

llemacs--path-logs-debug-pj
Log file paths for debug for current project. (= ‘llemacs--path-pj-logs-debug’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/debug.log"

llemacs--buf-main-sys
Name of main buffer.
Value: "*LLEMACS-MAIN*"

llemacs--path-logs-sys
Directory for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs"

llemacs--path-pj-results-figures
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/results/figures"

llemacs--logging-refresh-interval
Interval in seconds for auto-refreshing log buffer.
Value: 5

llemacs--path-logs-error-pj
Log file paths for error for current project. (= ‘llemacs--path-pj-logs-error’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/error.log"

llemacs--path-pj-results-tables
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/results/tables"

llemacs--buf-debug-sys
Buffer for debug output when debug mode enabled.
Value: "*LLEMACS-DEBUG*"

llemacs--path-pj
Root directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects"

llemacs--llm-api-min-delay
Minimum delay between API calls in seconds.
Value: 1.0

llemacs--llm-prompt-recipes
List of prompt recipe definitions loaded from recipe files.
Value: nil

llemacs--buf-logging-pj
Name of log buffer.
Value: "*LLEMACS-LOGGING ()*"

llemacs--path-logs-warn-pj
Log file paths for warn for current project. (= ‘llemacs--path-pj-logs-warn’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/warn.log"

llemacs--path-res-prompt-recipes
Directory for prompt template recipes.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/recipes"

llemacs--llm-gemini-script
Python script for calling Gemini
Value: "/home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/03-core-gemini_call.py"

llemacs--path-pj-logs-warn
Log file paths for warn for current project. (= ‘llemacs--path-logs-warn-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/warn.log"

llemacs--path-logs-debug-sys
Path to LLEMACS system Debug level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/debug.log"

llemacs--buf-prompt-pj
Buffer for prompt operations.
Value: "*LLEMACS-PROMPT ()*"

llemacs--git-auto-commit
Whether to automatically commit changes.
Value: t

llemacs-llm-provider
Switcher for LLM provider
Value: "google"

llemacs--path-pj-logs-by-level
Directory for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level"

llemacs--path-pj-logs-error
Log file paths for error for current project. (= ‘llemacs--path-logs-error-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/error.log"

llemacs--script-header-templates
Templates for file headers.
Value: ((css . "/* Timestamp: \"%s (%s)\" */
/* File: %s */

") (py . "#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: \"%s (%s)\"
# File: %s

__file__ = \"%s\"

") (el . ";;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: %s
;;; Timestamp: <%s>
;;; File: %s
") (sh . "#!/bin/bash
# Timestamp: \"%s (%s)\"
# File: %s

THIS_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"

") (yaml . "# Timestamp: \"%s (%s)\"
# File: %s
") (org . "# Timestamp: \"%s (%s)\"
# File: %s

") (md . "
") (tex . "%% Timestamp: \"%s (%s)\"
%% File: %s

"))

llemacs--path
Base directory for LLEMACS project.
Value: "/home/ywatanabe/proj/llemacs/"

llemacs--git-gitignore-path
Path to global gitignore template file.
Value: "~/.git-templates"

llemacs--path-projects
Directory for LLEMACS projects.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects"

llemacs--path-logs-elisp-pj
Log file paths for elisp for current project. (= ‘llemacs--path-pj-logs-elisp’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/elisp.log"

llemacs--path-logs-by-level-sys
Directory for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level"

llemacs--run-start-tag
Starting tag for Llemacs run
Value: "
================================================================================
Llemacs-run called.
================================================================================"

llemacs--path-res-tools
Directory for LLEMACS tools.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/tools"

llemacs--path-pj-reports
Reports directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/reports"

llemacs--path-pj-results
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/results"

llemacs--git-enabled
Whether to enable Git integration.
Value: t

llemacs--path-sample-project-zip
Sample project zip file for initializing project structure.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/000-sample-project.zip"

llemacs--buf-logging-sys
Name of log buffer.
Value: "*LLEMACS-LOGGING*"

llemacs--path-pj-logs-debug
Log file paths for debug for current project. (= ‘llemacs--path-logs-debug-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/debug.log"

llemacs--log-entry-limit
Maximum number of log entries to include in the context.
Value: 7

llemacs--llm-api-key-google
API key for Google Claude.
Value: "[MASKED]"

llemacs-project-max-history
Maximum number of project history entries to keep.
Value: 50

llemacs--path-logs-backup-sys
Directory for LLEMACS system logs backup.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/backup"

llemacs-project-default-name
Default project name when none specified.
Value: "default"

llemacs--path-logs-success-pj
Log file paths for success for current project. (= ‘llemacs--path-pj-logs-success’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/success.log"

llemacs--llm-api-retries
Number of retries for failed API requests.
Value: 3

llemacs--path-pj-python-env
Python environment path for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/.env"

llemacs--script-header-patterns
Regular expression patterns to match file headers.
Value: ((css . "\\`\\(?:/\\* Timestamp:.*\\*/
\\)?\\(?:/\\* File:.*\\*/
\\)?") (py . "\\`\\(?:#!.*
\\)?\\(?:# -\\*-.*-\\*-
\\)?\\(?:# Timestamp:.*
\\)?\\(?:# File:.*
\\)?\\(?:
\\)?\\(?:__file__.*
\\)?
?") (el . "\\`\\(?:;;;.*
\\)\\{1,4\\}") (sh . "\\`\\(?:#!.*
\\)?\\(?:# Timestamp:.*
\\)?\\(?:# File:.*
\\)?\\(?:
\\)?\\(?:THIS_DIR=.*
\\)?
?") (yaml . "\\`\\(?:# Timestamp:.*
\\)?\\(?:# File:.*
\\)?") (org . "\\`\\(?:# Timestamp:.*
\\)?\\(?:# File:.*
\\)?") (md . "\\`\\(?:
\\)") (tex . "\\`\\(?:%% Timestamp:.*
\\)?\\(?:%% File:.*
\\)?"))

llemacs--log-levels-sys
System-level logging-levels
Value: ((debug 0 . "Debug level logs") (info 1 . "Information level logs") (success 1 . "Success level logs") (prompt 1 . "Prompt operation logs") (elisp 1 . "Elisp execution logs") (api 1 . "API interaction logs") (search 1 . "Search operation logs") (warn 2 . "Warning level logs") (error 3 . "Error level logs"))

llemacs--path-pj-logs-success
Log file paths for success for current project. (= ‘llemacs--path-logs-success-pj’)
See ‘llemacs--path-logs-update-pj’.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/logs/by_level/success.log"

llemacs--timestamp-format
Timestamp references by Llemacs.
Value: "%Y-%m%d-%H%M%S"

llemacs--buf-main-pj
Name of main buffer.
Value: "*LLEMACS-MAIN ()*"

```
--------------------
====================
REQUESTS
====================
# Request: code-elisp-progn-block
* Using the context above, return only ONE ELISP PROGN CODE BLOCK, using the `progn` command
--------------------
# Request: project-management-mermaid
* Using the above context, return only ONE ELISP PROGN CODE BLOCK, using the `progn` command to update the project_management.mmd (`llemacs--path-pj-pm-mmd`)
--------------------
====================
