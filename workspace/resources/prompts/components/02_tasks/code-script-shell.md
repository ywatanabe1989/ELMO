<!-- ---
!-- title: 2025-01-06 08:16:30
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/code-script-shell.md
!-- --- -->

# Task: code-script-bash
* This system is working on a Linux distribution
* Generate BASH SCRIPTS (`.sh` file) when they improve project efficiency
* Save bash script files under this dedicated directory: 
  * `(expand-file-name "shell" llemacs--path-pj-scripts)`
* Write test code as well under this dedicated test directory:
  * `(expand-file-name "shell/test" llemacs--path-pj-scripts)`
* Details (Environment variables, commands, bash script template) are instructed in the rules section below