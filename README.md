<!-- ---
!-- title: ./Ninja/README.md
!-- author: ywatanabe
!-- date: 2024-12-18 04:49:48
!-- --- -->


# Ninja — Neural Information Network of Journaling Agents

(THIS REPOSITORY IS CURRENTLY UNDER ACTIVE DEVELOPMENT)

## Introduction
Ninja is an LLM agent system run on Emacs, which offers unique characteristics:
- Full CUI operations with rich GUI
- Interfaces for hacking tools seasoned by history
- Self-evolving potentials inherited from Emacs and Elisp

Here, we reintroduce Emacs — born in MIT's AI Lab in 1970s — as a catalyst for AI agents.

## Quick Start
```bash
git clone https://github.com/user/Ninja.git ~/.emacs.d/Ninja
```

## Apptainer

``` bash
apptainer build \
    --sandbox \
    --fakeroot \
    ./.apptainer/ninja/ninja.sandbox \
    ./.apptainer/ninja/ninja.def \
    2>&1 | tee ./.apptainer/ninja/ninja.sandbox.log

apptainer run \
    --writable \
    --fakeroot \
    ./.apptainer/ninja/ninja.sandbox
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp
```
