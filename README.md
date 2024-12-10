<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
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
# Clone repositories
git clone https://github.com/user/ninja.git ~/.emacs.d/ninja
git clone https://github.com/user/ninja-utils.git ~/.ninja/utils
```

## Developing
``` bash
apptainer_build_def2sand ./.apptainer/ninja/ninja.def
apptainer run -f ./.apptainer/ninja/ninja.sandbox
# ./.apptainer/ninja/ninja.sandbox.log
# apptainer run --fakeroot ./.apptainer/ninja/ninja.sandbox # not working yet

# Running Emacs from the Ninja user
./src/shell/launch_emacs.sh
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp


source /opt/Ninja/src/apptainer_builders/ENVS.sh.src
/opt/Ninja/src/apptainer_builders/create_ninja_user.sh
echo $NINJA_USER
groupadd -g 1000 ninja
useradd -m -u 1000 -g ninja -s /bin/bash ninja
echo "ninja:ninja" | chpasswd
usermod -aG sudo ninja
echo "ninja ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/ninja
chmod 0440 /etc/sudoers.d/ninja
mkdir -p /home/ninja
chown -R ninja:ninja /home/ninja
