<!-- ---
!-- title: 2024-12-23 21:38:42
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.emacs.d/lisp/Ninja/README.md
!-- --- -->

# ELMO: Emacs LLM Operations

## Apptainer

``` bash
./main.sh -m build
./main.sh -m shell

export NINJA_BIND="$HOME:/host:ro,$NINJA_ROOT_HOST/workspace:/workspace"


export NINJA_BIND="$PWD/workspace:/workspace"
./main.sh -m shell


export APPTAINER_BIND=

TGT_DIR="/tmp"
export NINJA_BIND="$PWD/apptainer/utils:$TGT_DIR"
./main.sh -m exec $TGT_DIR/check_ninja_envs.sh

ln -sfr ~/.dotfiles/.emacs.d/lisp/Ninja/apptainer/ninja.sandbox/workspace


./main.sh -m exec ./apptainer/utils/check_ninja_envs.sh
./main.sh -m exec env | grep ^NINJA_
# NINJA_LLM_API_KEY=
# NINJA_LLM_ENGINE=
# NINJA_ROOT_APPTAINER=/opt/Ninja
# NINJA_BASE_UID=9999
# NINJA_HOST_WORKSPACE=/home/ywatanabe/.emacs.d/lisp/Ninja/config/env/../../workspace
# NINJA_GROUP=ninja
# NINJA_ROOT_HOST=/home/ywatanabe/.emacs.d/lisp/Ninja/config/env/../..
# NINJA_ROOT=/home/ywatanabe/.emacs.d/lisp/Ninja/config/env/../..
# NINJA_HOST_HOME=/home/ywatanabe
# NINJA_N_AGENTS=2
# NINJA_HOST_USER=ywatanabe
# NINJA_HOME=/opt/Ninja


rm /workspace/*/*_backup*

tree --gitignore -L 3 /workspace

export NINJA_BIND="$HOME:/host:ro,$PWD/workspace:/workspace"
./main.sh -m shell

## Dev Links

``` bash
./src/apptainer_builders/rsync_from_sandbox.sh
./.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders
```


## Apptainer

``` bash
pkill -f "emacs --daemon=/home/ninja"
./src/shell/init_project.sh -p hello-world
./run.sh -m shell
./run.sh -m exec 
```

./run.sh -m shell
su ninja-001
ls -al
env | grep NINJA_
unset NINJA_EMACS_SERVER_SOCKET_DIR
unset NINJA_EMACS_SERVER_CHECK_INTERVAL
unset NINJA_API_TIMEOUT
unset NINJA_API_TIMEOUT
unset NINJA_MAX_RETRIES
unset NINJA_SHARED_BACKUPS
unset NINJA_SHARED_CONFIG
unset NINJA_BASE_DOT_EMACS




# 1 Apptainer 1 User

``` bash
# Set base permissions
chmod 755 /workspace
chmod 750 /workspace/ninjas
chmod 750 /workspace/projects
chmod 750 /workspace/resources

# Configure ninja homes
chmod 700 /workspace/ninjas/ninja-001/home
chmod 700 /workspace/ninjas/ninja-002/home

# Project permissions
chmod 770 /workspace/projects/000-PROJECTNAME

# Resources with read-only binding
# When launching containers:
apptainer run \
    --bind "/workspace/resources/private:/workspace/resources/private:ro" \
    --bind "/workspace/resources/shared:/workspace/resources/shared:ro" \
    --bind "/workspace/ninjas/ninja-001/home:/home/ninja" \
    --bind "/workspace:/workspace" \
    instance1.sif
    ```
    
    