<!-- ---
!-- title: 2025-01-04 18:00:02
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-format-elisp.md
!-- --- -->

# Rule: code-formatting-elisp
* Return only ONE ELISP CODE BLOCK, using the `progn` command
* Elisp code must be wrapped with the triplet code block tag: ```elisp\n(progn\nYOUR CODE\n)\n```
* Each Elisp code block must be self-contained, complete, and independently executable.
* External tools (shell commands, python, latex, and actually anything) can be accessible as long as your code is written in Elisp