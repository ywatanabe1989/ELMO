<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
!-- --- -->

# Ninja — Neural Information Network of Joint Agents
<p align="center">
  <img src="./docs/logo.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_01.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_02.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_03.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_04.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_05.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_06.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_07.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_08.jpg" width="250px" alt="./docs/logo.jpg">
  <img src="./docs/logos/log_09.jpg" width="250px" alt="./docs/logo.jpg">  
  <img src="./docs/logos/log_10.jpg" width="250px" alt="./docs/logo.jpg">    
</p>


THIS REPOSITORY IS CURRENTLY UNDER ACTIVE DEVELOPMENT
=====================================================

## Introduction
Ninja is an LLM agent system run on Emacs, which offers unique characteristics:
- Full CUI operations with rich GUI
- Interfaces for hacking tools seasoned by history
- Self-evolving potentials inherited from Emacs and Elisp

Here, we reintroduce Emacs — born in MIT's AI Lab in 1970s — as a catalyst for AI agents.

## Quick Start
```bash
# Clone repositories
git clone https://github.com/user/ninja.git ~/.emacs.d/ninja
git clone https://github.com/user/ninja-utils.git ~/.ninja/utils
```

## Developing
``` bash
apptainer_build_def2sand ./.apptainer/ninja/ninja.def
less ./.apptainer/ninja/ninja.sandbox.log
apptainer run ./.apptainer/ninja/ninja.sandbox
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp
