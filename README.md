<!-- ---
!-- title: ./Ninja/README.md
!-- author: ywatanabe
!-- date: 2024-12-19 22:52:17
!-- --- -->


# Ninja — Networked Intelligence of JASON Agents
An LLM agent system on Emacs

## Quick Start
```bash
git clone https://github.com/user/Ninja.git ~/.emacs.d/Ninja
```

## Apptainer

``` bash


APPTAINERENV_BIND="/workspace:/home/ywatanabe/.emacs.d/lisp/Ninja/workspace"
pkill -f "emacs --daemon=/home/ninja"
./run.sh -m exec /opt/Ninja/src/apptainer_builders/start_emacs.sh &
./run.sh -m exec emacsclient -s /home/ninja-001/.emacs.d/emacs-server/server --eval '(message "hello world")'
./run.sh -m exec emacsclient -s /home/ninja-002/.emacs.d/emacs-server/server --eval '(find-file "/workspace/abv.txt")'
./run.sh -m exec emacsclient -s /home/ninja-003/.emacs.d/emacs-server/server --eval '(find-file "/workspace/aaa.txt")'
```


./src/apptainer_builders/rsync_from_sandbox.sh
