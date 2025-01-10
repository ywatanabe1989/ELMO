<!-- ---
!-- Timestamp: 2025-01-10 11:44:41
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/results-report.md
!-- --- -->

# Rule: results-report
* Reports should be saved as this org file: 
  * '(expand-file-name "report.org" (expand-file-name (llemacs-timestamp) llemacs--path-pj-results))'
* After the creation, the org report file should be rendered as PDF
  * '(expand-file-name "report.pdf" (expand-file-name (llemacs-timestamp) llemacs--path-pj-results))'
* Reports should include the followings in this order:
  * LINKS TO FILE PATH OR DIRECTORY (when the number of files is large) of:
    * The project home directory
    * The mermaid SVG file for project management (with rendering the PNG image inline)
    * Scripts produced in the step
    * Figures produced in the step (with rendering the images inline)
    * Tables produced in the step
    * Data produced in the step
    * Reports produced in the step
* Results should not include:
  * Logs
  * scripts them selves
* Show the org file in the 'llemacs--buf-main-pj' buffer using the following Elisp function: 
```elisp
(defun llemacs--buf-disp-main-pj (&optional file-path enable-q enable-readonly enable-org)
  "Display 'llemacs--buf-main-pj' BUFFER with specified parameters.

Example:
(llemacs--buf-disp-main-pj \"~/test.txt\" t t t)

Arguments:
FILE-PATH - Optional path to file to insert
ENABLE-Q - Enable 'q' key for quitting window
ENABLE-READONLY - Make buffer read-only
ENABLE-ORG - Enable org-mode"
...)
```
* Images should be displayed inline mode, using `(org-display-inline-images)` and `(org-redisplay-inline-images)`
  * `IMAGE_WIDTH` should be 400

* Expected report is like this:
``` org
#+TITLE: EEG Feature Extraction Report
#+DATE: 20250105-064405

* Project Directory
[[file:/home/ywatanabe/proj/llemacs/workspace/projects/077-dsp-project/]]

* Project Management Diagram
#+ATTR_HTML: :width 400
#+ATTR_LATEX: :float t :placement [H]
[[file:/home/ywatanabe/proj/llemacs/workspace/projects/077-dsp-project/project_management/project_management.mmd/project_management.png]]

* Scripts
[[file:/home/ywatanabe/proj/llemacs/workspace/projects/077-dsp-project/scripts/python/eeg_feature_extraction.py]]

* Results
#+ATTR_HTML: :width 400
#+ATTR_LATEX: :float t :placement [H]
[[file:/home/ywatanabe/proj/llemacs/workspace/projects/077-dsp-project/results/figures/eeg_features_20250105-064405.jpg]]
    ```
