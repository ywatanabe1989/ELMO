<!-- ---
!-- title: 2025-01-04 18:08:03
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-formatting-python.md
!-- --- -->

# Rule: code-formatting-python
* Save data to `llemacs--path-pj-data` with appropriate directory structure.
* Save Python script files under `llemacs--path-pj-scripts` with appropriate directory structure.
* Results should be saved under `llemacs--path-pj-results` with appropriate directory structure.
* Add explicit type hints
* Follow PEP8 style guide
* Include Numpy-style docstring
* Avoid try-except blocks as much as possible
* No trailing comments
* Format with black