<!-- ---
!-- title: 2024-12-22 16:28:23
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.emacs.d/lisp/Ninja/README.md
!-- --- -->

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