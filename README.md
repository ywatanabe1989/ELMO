<!-- ---
!-- title: 2024-12-31 02:30:49
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/README.md
!-- --- -->

# Llemacs — LLM Agents on Emacs
<img src="./docs/llemacs.gif" width="100" alt="Llemacs Logo">
Llemacs is a file-based LLM agent system written in Elisp

# Disclaimer
- This repository is currently under active development.
- We do not have any responsibility for unintended file changes created by this system.

# Architecture
<a href="./docs/charts/project_flow.png">
    <img src="./docs/charts/project_flow.gif" alt="Project Flow" width="800">
</a>

## Apptainer

``` bash
./main.sh -m build
./main.sh -m shell
./main.sh -m run
```

## Workspace Organization
[./docs/workspace_tree.txt](./docs/workspace_tree.txt)