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
==============================================
🥷 Neural Intelligence Network Joint Agents 🥷
==============================================
```

## Overview
Ninja is an LLM agent system darting through Emacs. Born from MIT's AI Lab heritage, Emacs serves as the perfect dojo for AI agents with its:
- Full CUI operations
- Visualization prowess
- Battle-tested interfaces
- Self-crafting nature

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

## Status
🚧 Currently under active development 

## Contact
ywatanabe@alumni.u-tokyo.ac.jp


<!-- ----------------------------------------
 !-- ``` bash
 !-- apptainer_build_def2sand ./.apptainer/ninja/definitions/ninja_v004.def
 !-- less ./.apptainer/ninja/ninja.sandbox.log
 !-- apptainer run ./.apptainer/ninja/ninja.sandbox
 !-- ``` -->
