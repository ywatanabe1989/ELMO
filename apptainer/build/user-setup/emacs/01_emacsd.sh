#!/bin/bash
# Time-stamp: "2024-12-22 20:53:44 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/user_create_emacsd.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source /opt/Ninja/config/env/00_all.env
source "$THIS_DIR"/../permissions/emacsd.src

emacsd_init() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        _emacsd_init_emacsd $ninja_id
        _emacsd_init_shared $ninja_id
        _emacsd_init_private $ninja_id
        _emacsd_init_server_dir $ninja_id
    done
}

_emacsd_init_emacsd() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    rm -rf $NINJA_HOME/.emacs.d >/dev/null
    mkdir -p $NINJA_HOME/.emacs.d >/dev/null
}

_emacsd_init_shared() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    for shared_resource in $NINJA_EMACSD_SHARED_RESOURCES; do
        src=$NINJA_EMACSD_SHARED/$shared_resource
        tgt=$NINJA_EMACSD_PRIVATE/
        ln -sf $src $tgt >/dev/null
    done
}

_emacsd_init_server_dir() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    mkdir -p $NINJA_EMACSD_SERVER_DIR >/dev/null
}

_emacsd_init_private() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # for private_resource in recentf history; do
    for private_resource in $NINJA_EMACSD_PRIVATE_RESOURCES; do
        tgt=$NINJA_EMACSD_PRIVATE/$private_resource
        rm $tgt >/dev/null
        touch $tgt >/dev/null
    done

    tree $NINJA_EMACSD_PRIVATE -L 3
}

emacsd_init



# EOF
