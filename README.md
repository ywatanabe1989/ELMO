<!-- ---
!-- title: 2024-12-31 10:07:30
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/README.md
!-- --- -->

# Llemacs — LLM Agents on Emacs
<img src="./docs/llemacs.gif" width="100" alt="Llemacs Logo">
Llemacs is a file-based LLM agent system implemented in Elisp

# Disclaimer
- This repository is under active development
  - The system is currently non-functional as sub-modules are in development
- We assume no responsibility for any unintended file modifications made by this system

# Architecture
<a href="./docs/project_flow/project_flow.png">
    <img src="./docs/project_flow/project_flow.gif" alt="Project Flow" width="800">
</a>

## Permission control via Apptainer

Apptainer provides permission control by restricting agent access to the workspace directory only.

``` bash
main -m build
main -m shell
main -m run
```

## Workspace Organization
[./docs/workspace_tree.txt](./docs/workspace_tree.txt)

## Contribution
Please share your idea in the Discussion section.

## Contact
Yusuke.Watanabe@unimelb.edu.au