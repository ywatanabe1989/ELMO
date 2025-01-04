<!-- ---
!-- title: 2025-01-05 01:37:46
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/results-formatting-org-report.md
!-- --- -->

# Rule: results-formatting-org-report
* Summarize all the outputs (e.g., code, data, images...) into an org file
* Insert the PNG version of the Mermaid project management diagram (`llemacs--path-pj-pm-mmd`) at the beginning of the org file
* Save the org file under `llemacs--path-pj-results` with appropriate directory structure.
* Show the org file into the buffer popup-displayed by this Elisp pre-defined function: 
```elisp
(defun llemacs--buf-disp-main-pj (&optional file-path enable-q enable-readonly enable-org)
  "Display BUFFER with specified parameters.

Example:
(llemacs--buf-disp-main-pj "~/test.txt" t t t)

Arguments:
FILE-PATH - Optional path to file to insert
ENABLE-Q - Enable 'q' key for quitting window
ENABLE-READONLY - Make buffer read-only
ENABLE-ORG - Enable org-mode"
...)
```
* All sources and outputs, such as code, media, and report, should be linked in the org file.
* Convert the org file to pdf after completion
* Images should be displayed inline mode, using `(org-display-inline-images)` and `(org-redisplay-inline-images)`
* `IMAGE_WIDTH` should be 400
* Insert the directory path of the org file at the top of the contents