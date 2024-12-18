#!/bin/bash
# Time-stamp: "2024-12-18 20:44:51 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_create_emacsd.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src
source "$(dirname $0)"/user_correct_permissions_emacsd.sh.src

emacsd_init() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        _emacsd_init_emacsd $ninja_id
        _emacsd_init_shared $ninja_id
        _emacsd_init_private $ninja_id
        _emacsd_init_server_dir $ninja_id 
    done
}

########################################
# .emacs.d
########################################
_emacsd_init_emacsd() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    rm -rf $NINJA_HOME/.emacs.d 2>&1 >/dev/null
    mkdir -p $NINJA_HOME/.emacs.d 2>&1 >/dev/null
}

########################################
# Shared
########################################
_emacsd_init_shared() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # for shared_resource in elpa init.el inits lisp custom.el; do
    for shared_resource in "$NINJA_EMACSD_SHARED_RESOURCES"; do            
        src=$NINJA_EMACSD_SHARED/$shared_resource
        tgt=$NINJA_EMACSD_PRIVATE/$shared_resource
        ln -sf $src $tgt
        # su - $NINJA_USER -c "ln -sf $src $tgt"
    done
}

########################################
# Server Directory
########################################
_emacsd_init_server_dir() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    mkdir -p $NINJA_EMACSD_SERVER_DIR
}

########################################
# Private Files
########################################
_emacsd_init_private() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # for private_resource in recentf history; do
    for private_resource in "$NINJA_EMACSD_PRIVATE_RESOURCES"; do                
        tgt=$NINJA_EMACSD_PRIVATE/$private_resource
        rm $tgt 2>&1 >/dev/null
        touch $tgt 2>&1 >/dev/null
    done
}

emacsd_init
emacsd_correct_permissions

# EOF
