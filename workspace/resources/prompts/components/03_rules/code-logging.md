<!-- ---
!-- title: 2025-01-04 18:05:51
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-logging.md
!-- --- -->

# Rule: code-logging
* Add logs using the following pre-defined Elisp functions:
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