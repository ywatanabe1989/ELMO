<!-- ---
!-- title: 2024-12-21 16:13:52
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/Ninja/README.md
!-- --- -->

# Ninja — Networked Intelligence of JASON Agents
An LLM agent system on Emacs

## Quick Start
```bash
git clone https://github.com/user/Ninja.git ~/.emacs.d/Ninja
```

## Dev Links

``` bash
./src/apptainer_builders/rsync_from_sandbox.sh
./.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders
```


## Apptainer

``` bash
chmod 777 -R /home/ywatanabe/.emacs.d/lisp/Ninja/workspace
export NINJA_BIND="/home/ywatanabe/.emacs.d/lisp/Ninja/workspace:/workspace"
export NINJA_N_AGENTS=1
pkill -f "emacs --daemon=/home/ninja"
./src/shell/init_project.sh -p hello-world
```