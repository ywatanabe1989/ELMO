<!-- ---
!-- title: 2024-12-29 22:13:49
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/elmo/README.md
!-- --- -->

# Llemacs

![LLeMacs Logo](./docs/llemacs.gif){width=100}

## Apptainer

``` bash
./main.sh -m build
./main.sh -m shell
./main.sh -m run
```

``` elisp
(elmo-run "make a simple plot")
(elmo-run "make a simple gif")
(elmo-run "make a simple audio")
```

Context
(wsl) elmo $ tree workspace/projects/000-PROJECTNAME/
workspace/projects/000-PROJECTNAME/
├── data
├── docs
│   ├── project-dynamic.json
│   └── project-static.json
├── forum.json
├── README.md
├── requirements.txt
├── results
└── scripts

5 directories, 5 files
(wsl) elmo $ 

cat "$elmo_home/.emacs.d/init.el"

export ELMO_BIND="$(pwd)/workspace:/workspace"
main.sh

main.sh -m shell
/opt/elmo/apptainer/build/startup/start_emacs.sh

/workspace/elmos/elmo-000/.emacs.d/init.el

## Workspace

``` plaintext
(wsl) Elmo $ tree workspace/
workspace/
├── elmos
│   ├── elmo-000
│   │   ├── home
│   │   ├── memory
│   │   ├── messages
│   │   │   ├── inbox
│   │   │   └── outbox
│   │   ├── profile.json
│   │   ├── projects
│   │   │   └── messages
│   │   │       ├── inbox
│   │   │       └── outbox
│   │   └── status.json
│   ├── elmo-001
│   │   ├── home
│   │   ├── memory
│   │   ├── messages
│   │   │   ├── inbox
│   │   │   └── outbox
│   │   ├── profile.json
│   │   ├── projects
│   │   │   └── messages
│   │   │       ├── inbox
│   │   │       └── outbox
│   │   └── status.json
├── logs
├── projects
│   └── 000-PROJECTNAME
│       ├── data
│       ├── docs
│       │   ├── project-dynamic.json
│       │   └── project-static.json
│       ├── forum.json
│       ├── README.md
│       ├── requirements.txt
│       ├── results
│       └── scripts
└── resources
    ├── agents
    ├── prompts
    │   └── 001-context-to-elisp-code.json
    ├── scripts
    │   └── json2md.sh
    ├── templates
    │   ├── agent-000.json
    │   ├── context-000.json
    │   ├── experience-000.json
    │   ├── knowledge-000.json
    │   ├── message-000.json
    │   ├── output-000.json
    │   ├── project-dynamic-000.json
    │   ├── project-static-000.json
    │   ├── prompt-000.json
    │   ├── README.md
    │   ├── resource-000.json
    │   └── tool-000.json
    └── tools
        ├── 001-elisp.json
        ├── 002-python.json
        ├── 003-git.json
        ├── 004-github.json
        └── 005-tree.json
```



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