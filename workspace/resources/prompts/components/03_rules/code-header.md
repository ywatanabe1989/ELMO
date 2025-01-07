<!-- ---
!-- title: 2025-01-06 15:03:39
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-header.md
!-- --- -->

# Rule: code-header

Headers will be automatically inserted and updated (as a before-save-hook) for files with the following extensions: `py`, `el`, `sh`, `yaml`, `org`, `md`, `tex`. 

Thus, do not include shebung on these types of files to avoid duplication.
