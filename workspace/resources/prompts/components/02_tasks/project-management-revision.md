<!-- ---
!-- Timestamp: 2025-01-10 20:21:40
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/project-management-revision.md
!-- --- -->

# Task: project-management-revision
* Revise and update the entire workflow, milestones and tasks for the project
* Carefully review tags in the mermaid file (`todo`, `inProgress`, and `done`). 
  * This is critical as this file is the central communication tool between agents and users. 
    * `done` tag is only assigned when the task is certainly completed; `done` tasks will be not focused for other agents.
    * The worst case is where the `done` tag is applied but the task is not completed. This will cause other agents to skip such tasks. So, please carefully scrutinize progress tags.
