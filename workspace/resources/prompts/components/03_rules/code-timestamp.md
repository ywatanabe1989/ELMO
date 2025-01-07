<!-- Time-stamp: "2025-01-06 18:04:11 (ywatanabe)" -->
<!-- File: code-timestamp.md -->




















<!-- ---
!-- title: 2025-01-06 12:03:35
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-tool-finding.md
!-- --- -->

## Rule: code-timestamp
Use the following timestamp setter/getter to keep consistency among meaningful steps.
- `(llemacs--timestamp-get)` (= `(llemacs-timestamp)`)
- `(llemacs--timestamp-set)`
  - These functions use the variable `llemacs--timestamp`
