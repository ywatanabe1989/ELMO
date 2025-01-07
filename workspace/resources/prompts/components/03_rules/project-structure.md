<!-- Time-stamp: "2025-01-06 19:36:12 (ywatanabe)" -->
<!-- File: project-structure.md -->

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
