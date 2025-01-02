<!-- ---
!-- title: 2025-01-03 03:28:22
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompt-templates/components/03_rules/code-format-python.md
!-- --- -->

# Rules: code-format-python
* Return only Python code blocks, using code block marker: ```python\nYOUR CODE\n```
* Python script files should be saved under `/workspace/projects/<full-project-name>/scripts/`, with appropriate directory structure.
* Results should be saved under `/workspace/projects/<full-project-name>/results/`, with appropriate directory structure.
* Add explicit type hints
* Follow PEP8 style guide
* Avoid try-except blocks when possible
* No trailing comments
* Format with black