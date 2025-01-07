<!-- ---
!-- title: 2025-01-05 09:07:20
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-formatting-shell.md
!-- --- -->

# Rule: code-formatting-shell
* Save data to `llemacs--path-pj-data` with appropriate directory structure.
* Save shell script files under `(expand-file-name "scripts" llemacs--path-pj-scripts_` with appropriate directory structure.
* Results should be saved under `llemacs--path-pj-results` with appropriate directory structure.
* Include shebang and script metadata
* Implement proper argument parsing
* Include logging functionality
* Use proper if-fi and for-do-done syntax