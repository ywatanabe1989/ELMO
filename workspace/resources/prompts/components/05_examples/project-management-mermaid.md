<!-- ---
!-- Timestamp: 2025-01-10 21:28:05
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/05_examples/project-management-mermaid.md
!-- --- -->

# Exmple Output: project-management-mermaid

``` elisp
(progn
(let ((content "
graph TD
    subgraph Project Description
        PROJ[\"Project:
        000-MNIST-classification\"]:::done
        GOAL[\"Goal:
        Develop CNN model achieving >98% accuracy\"]:::done
        STATUS[\"Status:
        Model Training Phase\"]:::inProgress
    end

    subgraph MS1[\" \"]
        MILESTONE1[\"Milestone 1:
        Data Preparation\"]:::done
        MS1-Task1[Download MNIST]:::done
        MS1-Task2[Preprocess Data]:::done
        MS1-Task3[Create DataLoader]:::done
    end

    subgraph MS2[\" \"]
        MILESTONE2[\"Milestone 2:
        Model Development\"]:::inProgress
        MS2-Task1[CNN Architecture]:::done
        MS2-Task2[Training Loop]:::inProgress
        MS2-Task3[Hyperparameter Tuning]:::todo
    end

    subgraph MS3[\" \"]
        MILESTONE3[\"Milestone 3:
        Evaluation\"]:::todo
        MS3-Task1[Validation Metrics]:::todo
        MS3-Task2[Test Set Evaluation]:::todo
        MS3-Task3[Performance Analysis]:::todo
    end

    MILESTONE1 --> MS1-Task1 --> MS1-Task2 --> MS1-Task3
    MILESTONE2 --> MS2-Task1 --> MS2-Task2 --> MS2-Task3
    MILESTONE3 --> MS3-Task1 --> MS3-Task2 --> MS3-Task3

    classDef title fill:#e6e6e6,stroke:#333,stroke-width:2px,font-size:16px
    classDef starttag fill:#cce5ff,stroke:#333,stroke-width:2px
    classDef done fill:#9f9,stroke:#333,stroke-width:2px
    classDef inProgress fill:#ff9,stroke:#333,stroke-width:2px
    classDef todo fill:#fff,stroke:#333,stroke-width:2px
    classDef directory fill:#efe,stroke:#333,stroke-width:1px
    classDef endtag fill:#fcc,stroke:#333,stroke-width:2px
"))
(write-region content nil llemacs--path-pj-pm-mmd)
(llemacs--pj-mermaid-compile)))
```
