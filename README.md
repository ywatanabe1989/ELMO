<!-- ---
!-- title: ./Ninja/README.md
!-- author: ywatanabe
!-- date: 2024-12-11 14:03:39
!-- --- -->


# Ninja — Neural Information Network of Journaling Agents

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
    
apptainer shell \
     --fakeroot \
     --writable \
     ./.apptainer/ninja/ninja.sandbox
# /opt/Ninja/src/apptainer_builders/check_ninja_environment.sh
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp

## Developing
``` bash
apptainer run -f ./.apptainer/ninja/ninja.sandbox
# ./.apptainer/ninja/ninja.sandbox.log
# apptainer run --fakeroot ./.apptainer/ninja/ninja.sandbox # not working yet

# Running Emacs from the Ninja user
./src/shell/launch_emacs.sh
```

``` bash
source /opt/Ninja/src/apptainer_builders/ENVS.sh.src

/home/ywatanabe/.emacs.d/lisp/Ninja/src/apptainer_builders/user_create_python_env.sh


## HOME
ls /home/ -alt
ls /home/ninja-001/ -al
cat /home/ninja-001/.bashrc
ls /home/ninja-001/.emacs.d
cat /home/ninja-001/.emacs.d/init.el
tree /workspace


su ninja-001
ls /home/ninja-001 -al

echo $NINJAS_GROUP

echo $NINJA_USER
echo $NINJA_WORKSPACES
ls $NINJA_WORKSPACES
# apptainer shell --fakeroot --writable ./.apptainer/ninja/ninja.sandbox


# source /opt/Ninja/src/apptainer_builders/ENVS.sh.src
# /home/ywatanabe/.emacs.d/lisp/ninja/src/apptainer_builders/create_ninja_user.sh

# ../ninja.sandbox.log
```
