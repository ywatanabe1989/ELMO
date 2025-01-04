<!-- ---
!-- title: 2025-01-04 18:32:11
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/results-formatting-org-report.md
!-- --- -->

# Rule: results-formatting-org-report
* Summarize all the outputs (e.g., code, data, images...) into an org file
* Save the org file under `llemacs--path-pj-results` with appropriate directory structure.
* Show the org file into the buffer popup-displayed by this Elisp pre-defined function: 
  ```eslip
(defun llemacs--buf-disp-main (file-path enable-q enable-readonly enable-org)
  "Display BUFFER with specified parameters.

Example:
(llemacs--buf-disp-main \"*test*\" \"~/test.txt\" t t t)

Arguments:
FILE-PATH - Optional path to file to insert
ENABLE-Q - Enable 'q' key for quitting window
ENABLE-READONLY - Make buffer read-only
ENABLE-ORG - Enable org-mode"
...)
```
* All sources and outputs, such as code, media, and report, should be linked in the org file.
* Convert the org file to pdf after completion
* Images should be displayed inline mode, using `(org-toggle-inline-images 1)`
* `IMAGE_WIDTH` should be 400
* Insert the directory path of the org file at the top of the contents
<!-- * Example report is as follows:
 !--   * (Not genera -->