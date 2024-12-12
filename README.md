<!-- ---
!-- title: ./Ninja/README.md
!-- author: ywatanabe
!-- date: 2024-12-11 14:23:21
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
su ninja-001


su ninja-001 && cd 
su ninja-002
echo $NINJA_HOME
# no such directory: /home/ninja-001
ls ~
ls $HOME

echo $TERM
export TERM=xterm-256color
clear      # Clear screen
reset      # Reset terminal
echo -e '\033c'  # ANSI escape sequence to clear



su ninja-001



apptainer run -f ./.apptainer/ninja/ninja.sandbox
# ./.apptainer/ninja/ninja.sandbox.log
# apptainer run --fakeroot ./.apptainer/ninja/ninja.sandbox # not working yet

# Running Emacs from the Ninja user
./src/shell/launch_emacs.sh
```
