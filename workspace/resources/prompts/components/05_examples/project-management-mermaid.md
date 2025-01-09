<!-- ---
!-- Timestamp: 2025-01-09 19:35:43
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/examples/project-management-mermaid.md
!-- --- -->

# Exmple Output: project-management-mermaid

``` elisp
(progn
  (find-file llemacs--path-pj-pm-mmd)
  (erase-buffer)
  (insert "graph TD
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
PJSTATUS[In Progress]:::inProgress
end
subgraph PDIR[Project Directory]
Root[\"/workspace/projects/090-demo-project\"]:::directory
Config[config/]:::directory
Data[data/]:::directory
Scripts[scripts/]:::directory
Results[results/]:::directory
Resources[resources/]:::directory
PM[project_management.mmd]:::directory
end
end
subgraph PMFLOW[Project Management Flow]
MS1[Setup]:::done
MS2[Implementation]:::inProgress
MS3[Testing]:::todo
subgraph Tasks M1
T1[Initialize Project]:::done
T2[Setup Environment]:::done
end
subgraph Tasks M2
T3[Core Features]:::inProgress
T4[Documentation]:::todo
end
subgraph Tasks M3
T5[Unit Tests]:::todo
T6[Integration Tests]:::todo
end
end
MS1 --> T1
MS1 --> T2
MS2 --> T3
MS2 --> T4
MS3 --> T5
MS3 --> T6
classDef starttag fill:#cce5ff,stroke:#333,stroke-width:2px;
classDef done fill:#9f9,stroke:#333,stroke-width:2px;
classDef inProgress fill:#ff9,stroke:#333,stroke-width:2px;
classDef todo fill:#fff,stroke:#333,stroke-width:2px;
classDef directory fill:#efe,stroke:#333,stroke-width:1px;
classDef endtag fill:#fcc,stroke:#333,stroke-width:2px;")
  (save-buffer)
  (llemacs--pj-mermaid-compile)
  (kill-buffer))
```


<!-- # Exmple: project-management-mermaid
 !-- * This is an example of mermaid file for a project:
 !-- ```mermaid
 !-- graph TD
 !--     subgraph Legend
 !--         Z1[Todo]:::todo
 !--         Z2[In Progress]:::inProgress
 !--         Z3[Done]:::done
 !--         Z4[Directory]:::directory
 !--     end
 !--     subgraph Project Structure
 !--     subgraph PD[Project Description]
 !--         PJNAME[Project Name]:::done
 !--         PJGOALS[Goals]:::done
 !--         PJSTATUS[TODO]:::todo
 !--     end
 !--     subgraph PDIR[Project Directory]
 !--         Root["\/workspace\/projects\/000-sample-project"]:::directory
 !--         Config[config/]:::directory
 !--         Data[data/]:::directory
 !--         Scripts[scripts/]:::directory
 !--         Results[results/]:::directory
 !--         Resources[resources/]:::directory
 !--         Env[.env/]:::directory
 !--         Git[.git/]:::directory
 !--         Requirements[requirements.txt/]:::directory
 !--         Log[Log.txt/]:::directory
 !--         PM[project_management.mmd]:::directory
 !--     end
 !--     end
 !--     subgraph Execution Flow
 !--     subgraph Step
 !--         D[Compile Context]:::todo
 !--         E[Generate Elisp]:::todo
 !--         F[Execute Elisp]:::todo
 !--         G{Success?}:::todo
 !--     end
 !--     subgraph "Logging, Version Control, and State Update"
 !--         H[Log Success]:::todo
 !--         I[Log Error]:::todo
 !--         J{Milestone?}:::todo
 !--         K[Git Commit]:::todo
 !--         L[Log Only]:::todo
 !--         M{Goal Met?}:::todo
 !--         N[Update Project_States]:::todo
 !--     end
 !--     end
 !--     subgraph PMFLOW[Project Management Flow]
 !--         MS1[Milestone 1]:::done
 !--         MS2[Milestone 2]:::todo
 !--     subgraph Tasks M1
 !--         T1[task1]:::done
 !--         T2[task2]:::done
 !--     end
 !--     subgraph Tasks M2
 !--         T3[task1]:::todo
 !--         T4[task2]:::todo
 !--     end
 !--     end
 !--     Start[Start]:::starttag -\-> PD
 !--     PD -\-> PDIR
 !--     PM -\-> PMFLOW
 !--     PMFLOW -\-> PM
 !--     PDIR -\-> D
 !--     D -\-> E -\-> F -\-> G
 !--     G -- Yes -\-> H
 !--     G -- No -\-> I
 !--     H -\-> J
 !--     J -- Yes -\-> K
 !--     J -- No -\-> L
 !--     K -\-> M
 !--     I -\-> L
 !--     L -\-> M
 !--     M -- No -\-> N
 !--     N -\-> Root
 !--     M -- Yes -\-> End[End]:::endtag
 !--     PJGOALS -\-> PMFLOW
 !--     MS1 -\-> T1
 !--     MS1 -\-> T2
 !--     MS2 -\-> T3
 !--     MS2 -\-> T4
 !--     classDef starttag fill:#cce5ff,stroke:#333,stroke-width:2px;
 !--     classDef done fill:#9f9,stroke:#333,stroke-width:2px;
 !--     classDef inProgress fill:#ff9,stroke:#333,stroke-width:2px;
 !--     classDef todo fill:#fff,stroke:#333,stroke-width:2px;
 !--     classDef directory fill:#efe,stroke:#333,stroke-width:1px;
 !--     classDef endtag fill:#fcc,stroke:#333,stroke-width:2px;
 !--     class Root,Config,Data,Scripts,Results,Resources directory;
 !-- ``` -->

