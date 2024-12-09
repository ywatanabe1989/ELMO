<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
!-- --- -->

``` plaintext
 ███╗   ██╗██╗███╗   ██╗     ██╗ █████╗ 
 ████╗  ██║██║████╗  ██║     ██║██╔══██╗
 ██╔██╗ ██║██║██╔██╗ ██║     ██║███████║
 ██║╚██╗██║██║██║╚██╗██║██   ██║██╔══██║
 ██║ ╚████║██║██║ ╚████║╚█████╔╝██║  ██║
 ╚═╝  ╚═══╝╚═╝╚═╝  ╚═══╝ ╚════╝ ╚═╝  ╚═╝
==========================================
Neural Information Network of Joint Agents
==========================================
```

THIS REPOSITORY IS CURRENTLY UNDER ACTIVE DEVELOPMENT
=====================================================

## Introduction
Ninja is an LLM agent system run on Emacs, which offers unique characteristics:
- Full CUI operations with rich rendering functionalities 
- Interfaces for hacking tools seasoned by history
- Self-modifying system that reads, writes, and executes its own Elisp code

Here, we reintroduce Emacs — born in MIT's AI Lab in 1970s — as a basis for AI agents.

## Quick Start
```bash
# Clone repositories
git clone https://github.com/user/ninja.git ~/.emacs.d/ninja
git clone https://github.com/user/ninja-utils.git ~/.ninja/utils

# Build and run container
cd ~/.ninja
apptainer build ninja.sif ninja.def
apptainer run ninja.sif
```


## Contact
ywatanabe@alumni.u-tokyo.ac.jp


<!-- ----------------------------------------
 !-- ``` bash
 !-- apptainer_build_def2sand ./.apptainer/ninja/definitions/ninja_v004.def
 !-- less ./.apptainer/ninja/ninja.sandbox.log
 !-- apptainer run ./.apptainer/ninja/ninja.sandbox
 !-- ``` -->
