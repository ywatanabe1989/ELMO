<!-- ---
!-- title: 2024-12-31 02:18:13
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/docs/dev/concepts.md
!-- --- -->

## Core Concepts

| Unit          | Description                                       |
|---------------|---------------------------------------------------|
| **Context**   | Project, Milestone, Task, Process, Thread, Branch |
| **LLM**       | Translates Context to Code                        |
| **Execution** | Executes Code, generating Output                  |
| **Feedback**  | Captures Stdout, Stderr, and Exit codes           |

| Basic               | Description                                   |
|---------------------|-----------------------------------------------|
| **Permissions**     | Roles and access controls.                    |
| **Validation**      | Data integrity checks, JSON Schema Validation |
| **Version Control** | Tracked using Git.                            |

| Journaling method | Description                                              |
|-------------------|----------------------------------------------------------|
| **Message**       | Interchangeable JSON and Markdown protocols for logging. |

| Entity        | Description                                       |
|---------------|---------------------------------------------------|
| **Project**   | Top-level grouping of tasks and resources.        |
| **Milestone** | Goal within a project.                            |
| **Task**      | Atomic work unit within a milestone.              |
| **Agent**     | Autonomous process using tools to complete tasks. |

| Pool            | Description                                      |
|-----------------|--------------------------------------------------|
| **Projects**    | List of all active/inactive projects.            |
| **Agents**      | Available agent configurations and their status. |
| **Prompts**     | Library of LLM prompt templates.                 |
| **Tools**       | Definitions of available tools (Emacs, scripts). |
| **Resources**   | Files, links, data (read/writable) for agents.   |
| **Knowledge**   | Accumulated data from past experiences.          |
| **Experiences** | Logs of past executions and agent behavior.      |

| Action                    | Description                                            |
|---------------------------|--------------------------------------------------------|
| **Project Understanding** | Analyzes project descriptions to plan tasks.           |
| **Task Splitting**        | Divides projects/milestones into manageable tasks.     |
| **Context Building**      | Generates task context from available resources.       |
| **Code Generating**       | Creates Elisp/tool invocations based on task/context.  |
| **Evaluating**            | Assesses execution success/failure using feedback.     |
| **Git Managing**          | Handles version control for configuration, code, docs. |
| **Reporting**             | Generates Markdown reports on progress.                |
| **Maintaining**           | Keeps system up-to-date (Git, Apptainer, Elisp).       |
| **Allocating**            | Assigns agents and resources to tasks.                 |