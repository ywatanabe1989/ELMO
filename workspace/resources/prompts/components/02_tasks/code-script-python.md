<!-- ---
!-- title: 2025-01-06 08:11:21
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/code-script-python.md
!-- --- -->

# Task: code-script-python
* Generate PYTHON SCRIPTS (`.py` files) when they improve project efficiency
* Save python script files under dedicated directory: 
  * `(expand-file-name "python" llemacs--path-pj-scripts)`
* Write test code as well under this dedicated test directory:
  * `(expand-file-name "pytyon/test" llemacs--path-pj-scripts)`
* Python environment, executable, and installed-packages will be explained in the rules section
* Python scripts are ALWAYS CALLED VIA ELISP EVALUATION in this system
  * Thus, python scripts have to use `argparse` to work with variables