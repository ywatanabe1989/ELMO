<!-- ---
!-- Timestamp: 2025-01-10 20:25:32
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/project-management-mermaid.md
!-- --- -->

# Rule: project-management-using-mermaid
* Project management is SOLELY handled by the mermaid file for the project.
* Breakdown complex tasks into small tasks. 
* Organize the progress and plans for the project into the mermaid file: `llemacs--path-pj-pm-mmd` (this file has the `.mmd` extension).
  * In the mermaid file, use state tracking tags (i.g., `todo`, `inProgress`, and `done`)
* The mermaid file is not for how to handle mermaid file but for the project itself.
* The mermaid file should include:
  * Project Description
  * Project Work Flow
  * Project Milestones
  * Project Tasks
  * The progress tags to visually indicate the project status
* The mermaid file should NOT include:
  * Project Directory
  * Legend
