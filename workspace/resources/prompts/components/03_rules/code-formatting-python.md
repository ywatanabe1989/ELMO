<!-- ---
!-- title: 2025-01-05 15:36:36
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-formatting-python.md
!-- --- -->

# Rule: code-formatting-python
* Use Python binary at `llemacs--path-python-sys`
* Save data to `llemacs--path-pj-data` with appropriate directory structure.
* Save Python script files under `(expand-file-name "python" llemacs--path-pj-scripts)`.
* Produced results from python script should be saved under `llemacs--path-pj-results`.