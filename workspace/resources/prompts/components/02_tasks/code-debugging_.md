<!-- ---
!-- title: 2025-01-06 08:06:20
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/code-debugging.md
!-- --- -->

# Task: code-debugging
* Add debugging statements to identify issues
    * For Python: use print statements
    * For Elisp: use the custom logging functions detailed in the rules section
      * Use the custom logging functions.
        * `(llemacs--logging-write-debug-pj message)`
        * `(llemacs--logging-write-warn-pj message)`
        * `(llemacs--logging-write-error-pj message)`
    * For Shell: use echo commands