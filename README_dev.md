<!-- ---
!-- title: 2024-12-23 23:30:53
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.emacs.d/lisp/Elmo/README_dev.md
!-- --- -->

/home/ywatanabe/.emacs.d/lisp/Elmo/docs/dev/workspace_structure.md
Prompts defines llm
/workspace/system-log (all)

for dir in ./workspace/elmos/elmo-*; do 
    echo $dir
mkdir -p $dir/memory
mkdir -p $dir/home    
touch $dir/status.json
mkdir -p $dir/messages/{inbox,outbox}    
mkdir -p $dir/projects/messages/{inbox,outbox}    
mkdir -p $dir/profile.json
done

touch workspace/projects/000-PROJECTNAME/forum.json

tree workspace

rm -rf 


## Apptainer

``` bash
./main.sh -m build
./main.sh -m shell



export ELMO_BIND="$HOME:/host:ro,$ELMO_ROOT_HOST/workspace:/workspace"


export ELMO_BIND="$PWD/workspace:/workspace"
./main.sh -m shell


export APPTAINER_BIND=

TGT_DIR="/tmp"
export ELMO_BIND="$PWD/apptainer/utils:$TGT_DIR"
./main.sh -m exec $TGT_DIR/check_elmo_envs.sh

ln -sfr ~/.dotfiles/.emacs.d/lisp/ELMO/apptainer/elmo.sandbox/workspace


./main.sh -m exec ./apptainer/utils/check_elmo_envs.sh
./main.sh -m exec env | grep ^ELMO_
# ELMO_LLM_API_KEY=
# ELMO_LLM_ENGINE=
# ELMO_ROOT_APPTAINER=/opt/elmo
# ELMO_BASE_UID=9999
# ELMO_HOST_WORKSPACE=/home/ywatanabe/.emacs.d/lisp/ELMO/config/env/../../workspace
# ELMO_GROUP=elmo
# ELMO_ROOT_HOST=/home/ywatanabe/.emacs.d/lisp/ELMO/config/env/../..
# ELMO_ROOT=/home/ywatanabe/.emacs.d/lisp/ELMO/config/env/../..
# ELMO_HOST_HOME=/home/ywatanabe
# ELMO_N_AGENTS=2
# ELMO_HOST_USER=ywatanabe
# ELMO_HOME=/opt/elmo


rm /workspace/*/*_backup*

tree --gitignore -L 3 /workspace

export ELMO_BIND="$HOME:/host:ro,$PWD/workspace:/workspace"
./main.sh -m shell

## Dev Links

``` bash
./src/apptainer_builders/rsync_from_sandbox.sh
./.apptainer/elmo/elmo.sandbox/opt/elmo/src/apptainer_builders
```


## Apptainer

``` bash
pkill -f "emacs --daemon=/home/elmo"
./src/shell/init_project.sh -p hello-world
./run.sh -m shell
./run.sh -m exec 
```

./run.sh -m shell
su elmo-001
ls -al
env | grep ELMO_
unset ELMO_EMACS_SERVER_SOCKET_DIR
unset ELMO_EMACS_SERVER_CHECK_INTERVAL
unset ELMO_API_TIMEOUT
unset ELMO_API_TIMEOUT
unset ELMO_MAX_RETRIES
unset ELMO_SHARED_BACKUPS
unset ELMO_SHARED_CONFIG
unset ELMO_BASE_DOT_EMACS




# 1 Apptainer 1 User

``` bash
# Set base permissions
chmod 755 /workspace
chmod 750 /workspace/elmos
chmod 750 /workspace/projects
chmod 750 /workspace/resources

# Configure elmo homes
chmod 700 /workspace/elmos/elmo-001/home
chmod 700 /workspace/elmos/elmo-002/home

# Project permissions
chmod 770 /workspace/projects/000-PROJECTNAME

# Resources with read-only binding
# When launching containers:
apptainer run \
--bind "/workspace/resources/private:/workspace/resources/private:ro" \
--bind "/workspace/resources/shared:/workspace/resources/shared:ro" \
--bind "/workspace/elmos/elmo-001/home:/home/elmo" \
--bind "/workspace:/workspace" \
instance1.sif
```