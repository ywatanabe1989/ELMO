<!-- ---
!-- title: 2025-01-05 16:13:05
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/examples/automatic-project-development.md
!-- --- -->

# Demo of Llemacs: local run without using Apptainer

## Automatic project development

``` elisp
(llemacs--pj-init 
    "MNIST" 
    "- MNIST classification
     - Multiple models using skleran (cpu-only)
     - MNIST clustering
     - Evaluation
     - Visualization
       - Confusion Matrix
       - Shilhouette Score
       - Classification Report
       ")


(defun develop ()
    (interactive)
    (progn
    (dotimes (i 7)
        (llemacs-pj-update-mermaid)
        (llemacs-pj-develop))))
        
        (develop)

;; Mermaid
(defun llemacs-pj-update-mermaid ()
(interactive)
    (llemacs-run 
"- Update the project_management.mmd (`llemacs--path-pj-pm-mmd`) based on the goals and progress so far
- Carefully review tags in the mermaid file (`todo`, `inProgress`, and `done`). This is critical as this file is only the communication tool between agents and users. So, `done` tag is only assigned when the task is certainly compeleted.
- For example, to add `done` tag, at lest one representative figure is representative figure.
- It is the worst case where the `done` tag is applied even when the task is not completed. This will cause other agents to skip such tasks. So, please carefully scrutinize progress tags.
- Split into small milestones and tasks, which consolidate the base of successful achievement of goals.
- Directory tree and past logs will be especially useful.
- Render the updated prorject_management.mmd as png and svg images.
- Summarize the images into report with org mode
- Open the created svg file using `$ wslview` or `$ google-chrome`
- Demo data should be saved under (`llemacs--path-pj-data`) for reuse
" 
"project-management" 
"error"))

;; Development
(defun llemacs-pj-develop ()
(interactive)
    (llemacs-run 
"- Advance the project baesd on the project_management.mmd (`llemacs--path-pj-pm-mmd`)
- Plot a representative figure which showcase what the python script does.
- Save python scripts under the scripts dir (`llemacs--path-pj-scripts`).
- Save Elisp code should be saved  under the scripts dir (`llemacs--path-pj-scripts`).
- Save outputs (e.g., jpg figures) under the results dir (`llemacs--path-pj-results`)
- Mermaid tags might be wrong. So, please check the directory tree carefully and determine which task you should work on.
- You can also edit the mermaid file.
- Summarize results as an org report with inline images displayed
- To save jpg image in python, this snippet will be helpful:
``` python
import Image
import matplotlib.pyplot as plt

plt.plot(range(10))
plt.savefig('testplot.png')
Image.open('testplot.png').save('testplot.jpg','JPEG')
```
" 
"code-gen" 
"error"))

```


;; Initialize a project: setting project-name and goals
;; (llemacs--pj-init 
;;     "Epilepsy-prediction-project" 
;;     "- Create a demo ECoG signals with 400 Hz and 16 channels
;;      - Create a demo script to classify preictal vs. interictal states
;;      - Evaluate the performances using balanced accuracy, with plotting confusion matrix
;; ")