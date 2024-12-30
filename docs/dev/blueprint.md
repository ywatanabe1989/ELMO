<!-- ---
!-- title: 2024-12-28 17:39:13
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/llemacs/docs/dev/blueprint.md
!-- --- -->

<!-- 1. REPL cycle
 !--    **Read**
 !--    - Takes input from user/system
 !--    - Parses into internal representation
 !-- 
 !--    **Evaluate**
 !--    - Processes the parsed input 
 !--    - Executes computations/logic
 !-- 
 !--    **Print**
 !--    - Shows results
 !--    - Formats output
 !-- 
 !--    **Loop**
 !--    - Returns to Read step
 !--    - Maintains state between iterations
 !-- 2. Model-View-Controller (MVC)
 !--    - Context = Model
 !--    - LLM = Controller
 !--    - Execution/Feedback = View
 !-- 
 !-- 3. Unix Philosophy
 !--    - Small, focused components
 !--    - Input/Output streams
 !--    - Composability
 !-- 
 !-- 4. CI/CD Pipeline structure
 !--    - Context = Environment setup
 !--    - LLM = Build
 !--    - Execution = Deploy
 !--    - Feedback = Monitor -->

**Unit (or Step):**
| Unit          | Description                                                         |
|---------------|---------------------------------------------------------------------|
| [ ] **Context**   | Project, Tasks, Progress, Status, Past log, resources               |
| [x] **LLM**       | Translates Context to Code                                          |
| [x] **Execution** | Executes Code, generating Output                                    |
| [ ] **Feedback**  | Produced outputs, Stdout, Stderr, and Exit codes, to update context |

**Updated Security Table:**
| Basic               | Description                |
|---------------------|----------------------------|
| **Permissions**     | Roles and access controls. |
| **Validation**      | Data integrity checks, JSON Schema Validation    |
| **Version Control** | Tracked using Git.         |

**Updated Journaling Table:**
| **Message** | Interchangeable JSON and Markdown protocols for logging. |

**Updated Entities and Status Table:**
| Entity    | Status        | Description |
|-----------|---------------|-------------|
| **Project** | Ready, Running, Pending, Finished | Top-level grouping of tasks and resources. |
| **Milestone**| Ready, Running, Pending, Finished | Goal within a project. |
| **Task**    | Ready, Running, Pending, Finished | Atomic work unit within a milestone. |
| **Agent**   | Ready, Running, Pending, Finished | Autonomous process using tools to complete tasks. |

**Updated Pools Table:**
| Pool            | Description                                      |
|-----------------|--------------------------------------------------|
| **Projects**    | List of all active/inactive projects.            |
| **Agents**      | Available agent configurations and their status. |
| **Prompts**     | Library of LLM prompt templates.                 |
| **Tools**       | Definitions of available tools (Emacs, scripts). |
| **Resources**   | Files, links, data (read/writable) for agents.   |
| **Knowledge**   | Accumulated data from past experiences.          |
| **Experiences** | Logs of past executions and agent behavior.      |

**Updated Actions Table:**
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