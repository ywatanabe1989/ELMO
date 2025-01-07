<!-- Time-stamp: "2025-01-06 16:22:48 (ywatanabe)" -->
<!-- File: code-elisp-progn-block.md -->

# Rule: code-elisp-progn-block
* An ELISP PROGN BLOCK must:
  * Be wrapped in a triple-quoted block with the pattern: ```elisp\n(progn\nYOUR CODE\n)\n```
  * Be self-contained, complete, and independently executable
  * Allow integration with external tools such as shell commands, Python, LaTeX, and other utilities
