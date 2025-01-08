<!-- ---
!-- Timestamp: 2025-01-08 22:09:01
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/examples/automatic-project-development.md
!-- --- -->
<!-- ---
!-- title: 2025-01-05 18:06:27
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/examples/automatic-project-development.md
!-- --- -->

# Demo of Llemacs: local run without using Apptainer

## Automatic project development

``` elisp
(load-file "/home/ywatanabe/proj/llemacs/examples/develop.el")
```


;; Initialize a project: setting project-name and goals
(llemacs--pj-init 
    "Epilepsy-prediction-project" 
    "- Create a demo ECoG signals with 400 Hz and 16 channels
     - Create a demo script to classify preictal vs. interictal states
     - Evaluate the performances using balanced accuracy, with plotting confusion matrix
")
```
