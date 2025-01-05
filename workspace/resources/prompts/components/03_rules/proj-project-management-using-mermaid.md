<!-- ---
!-- title: 2025-01-05 05:24:47
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/proj-project-management-using-mermaid.md
!-- --- -->

# Rule: proj-project-management-using-mermaid
* Understand the hierarchy of project components: goals, milestones, and tasks.
* Breakdown the goals into milestones. Upon achievement of milestone, `git commit` is performed.
* Breakdown milestones into manageable tasks.
* Breakdown complex tasks into small tasks. 
  * Tasks will be performed by agents, including yourself in the future, with feedback loop enabled.
  * Each task must be manageable with ONE ELISP CODE, often connected with the `progn` command. 
  * Thus, split the tasks when interaction is necessary. For example, when `ls <directory>`, a task should be stopped.
* Organize project progress/plans into this mermaid fil for project management: `llemacs--path-pj-pm-mmd` (this file has the `.mmd` extension).
  * In the mermaid file, use state tracking tags (i.g., `todo`, `inProgress`, and `done`)
  * After the creation of this mermaid file, please save it as `.png` and `.svg` files, with changing extensions from `.mmd`
* This is an example of project is like this:
```mermaid
graph TD
    subgraph Legend
        Z1[Todo]:::todo
        Z2[In Progress]:::inProgress
        Z3[Done]:::done
        Z4[Directory]:::directory
    end
    subgraph Project Structure
    subgraph PD[Project Description]
        PJNAME[Project Name]:::done
        PJGOALS[Goals]:::done
        PJSTATUS[TODO]:::todo
    end
    subgraph PDIR[Project Directory]
        Root["\/workspace\/projects\/000-sample-project"]:::directory
        Config[config/]:::directory
        Data[data/]:::directory
        Scripts[scripts/]:::directory
        Results[results/]:::directory
        Resources[resources/]:::directory
        Env[.env/]:::directory
        Git[.git/]:::directory
        Requirements[requirements.txt/]:::directory
        Log[Log.txt/]:::directory
        PM[project_management.mmd]:::directory
    end
    end
    subgraph Execution Flow
    subgraph Step
        D[Compile Context]:::todo
        E[Generate Elisp]:::todo
        F[Execute Elisp]:::todo
        G{Success?}:::todo
    end
    subgraph "Logging, Version Control, and State Update"
        H[Log Success]:::todo
        I[Log Error]:::todo
        J{Milestone?}:::todo
        K[Git Commit]:::todo
        L[Log Only]:::todo
        M{Goal Met?}:::todo
        N[Update Project_States]:::todo
    end
    end
    subgraph PMFLOW[Project Management Flow]
        MS1[Milestone 1]:::done
        MS2[Milestone 2]:::todo
    subgraph Tasks M1
        T1[task1]:::done
        T2[task2]:::done
    end
    subgraph Tasks M2
        T3[task1]:::todo
        T4[task2]:::todo
    end
    end
    Start[Start]:::starttag --> PD
    PD --> PDIR
    PM --> PMFLOW
    PMFLOW --> PM
    PDIR --> D
    D --> E --> F --> G
    G -- Yes --> H
    G -- No --> I
    H --> J
    J -- Yes --> K
    J -- No --> L
    K --> M
    I --> L
    L --> M
    M -- No --> N
    N --> Root
    M -- Yes --> End[End]:::endtag
    PJGOALS --> PMFLOW
    MS1 --> T1
    MS1 --> T2
    MS2 --> T3
    MS2 --> T4
    classDef starttag fill:#cce5ff,stroke:#333,stroke-width:2px;
    classDef done fill:#9f9,stroke:#333,stroke-width:2px;
    classDef inProgress fill:#ff9,stroke:#333,stroke-width:2px;
    classDef todo fill:#fff,stroke:#333,stroke-width:2px;
    classDef directory fill:#efe,stroke:#333,stroke-width:1px;
    classDef endtag fill:#fcc,stroke:#333,stroke-width:2px;
    class Root,Config,Data,Scripts,Results,Resources directory;
```
* Use these Elisp code if they are helpful.
``` elisp* 
(defun my/mermaid-compile ()
  "Generate PNG, SVG, and GIF after saving Mermaid files."
  (interactive)
  (when (and (eq major-mode 'mermaid-mode)
             (buffer-file-name))
    (let* ((input-file (buffer-file-name))
           (base-name (file-name-sans-extension input-file)))
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".png")
                    "--backgroundColor" "transparent")
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".svg")
                    "--backgroundColor" "transparent"
                    "-f" "svg")
      (call-process "convert" nil nil nil
                    (concat base-name ".png")
                    (concat base-name ".gif"))
      (message "Created %s.png, %s.svg and %s.gif" base-name base-name base-name))))

(defun my/mermaid-after-save-hook ()
  (my/mermaid-compile))

(use-package mermaid-mode
  :ensure t
  :custom
  (setq mermaid-output-format ".png")
  :hook
  (after-save . my/mermaid-after-save-hook))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path "/usr/local/bin/mmdc")
  (ob-mermaid-extra-args "--backgroundColor transparent -f svg"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)))
```