<!-- ---
!-- title: 2024-12-21 10:46:58
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
./run.sh -m exec /opt/Ninja/src/apptainer_builders/start_emacs.sh &
./run.sh -m exec emacsclient -s /home/ninja-001/.emacs.d/emacs-server/server --eval '(message "hello world")'
./run.sh -m exec emacsclient -s /home/ninja-001/.emacs.d/emacs-server/server --eval '(find-file "/workspace/abv.txt")'
./run.sh -m exec emacsclient -s /home/ninja-001/.emacs.d/emacs-server/server --eval '(find-file "/workspace/aaa.txt")'
```


# Set project name
export PROJECTNAME="hello-project"

# Copy template with proper numbering
cp -r projects/000-template projects/001-${PROJECTNAME}

# Replace placeholder text in files
find projects/001-${PROJECTNAME} -type f -exec sed -i "s/template/${PROJECTNAME}/g" {} +
find projects/001-${PROJECTNAME} -type f -exec sed -i "s/PROJECTNAME/${PROJECTNAME}/g" {} +

# Have agent initialize via emacsclient 
./run.sh -m exec emacsclient -s /home/ninja-001/.emacs.d/emacs-server/server --eval '
(progn
  (find-file "/workspace/projects/001-hello-project/docs/project.json")
  (find-file "/workspace/projects/001-hello-project/docs/context.md")
  (find-file "/workspace/projects/001-hello-project/README.md")
)'