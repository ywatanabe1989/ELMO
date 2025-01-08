<!-- ---
!-- Timestamp: 2025-01-08 19:26:03
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/project-structure.md
!-- --- -->

# Rule: project-structure
* Every project has a unique identifier in `<ID>-<name>` format
* Project paths are defined as Elisp variables with `llemacs--path-` prefix
* Project must include core directories:
  * `.env/`: Environment setup
  * `config/`: Configuration files
  * `data/`: Data storage
  * `docs/`: Document files
  * `logs/`: Logging system
  * `project_management/`: Project tracking
  * `results/`: Output files
  * `reports/`: Progress Summary
  * `scripts/`: Source code
