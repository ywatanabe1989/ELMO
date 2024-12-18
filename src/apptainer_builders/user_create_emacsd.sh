#!/bin/bash
# Time-stamp: "2024-12-18 05:32:40 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_create_emacsd.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

create_ninjas_emacsd() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_emacsd $ninja_id
        create_ninja_server_dir        
    done
}

create_ninja_emacsd() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # Initialization
    rm -rf $NINJA_HOME/.emacs.d 2>&1 >/dev/null
    mkdir -p $NINJA_HOME/.emacs.d 2>&1 >/dev/null

    # Shared configurations (symlink)
    for shared_resource in elpa init.el inits lisp custom.el; do
        tgt=$NINJA_EMACSD_PRIVATE/$shared_resource
        su - $NINJA_USER -c "ln -sf $NINJA_EMACSD_SHARED/$shared_resource $tgt"
        chown -R $NINJA_USER:$NINJAS_GROUP $tgt
        chmod -R 770 $tgt
    done

    # Private configurations
    for private_resource in recentf history; do
        tgt=$NINJA_EMACSD_PRIVATE/$private_resource
        rm $tgt 2>&1 >/dev/null
        touch $tgt 2>&1 >/dev/null
        chown $user:$user $tgt
        chmod 700 $tgt
    done
}

create_ninja_server_dir() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    mkdir -p $NINJA_EMACS_SERVER_DIR
    chown -R $user:$user $NINJA_EMACS_SERVER_DIR    
    chmod -R 700 $NINJA_EMACS_SERVER_DIR

    # Create symlinks
    for src in elpa init.el inits lisp custom.el; do
        su - $user -c "ln -sf $shared_emacsd/$src /home/$user/.emacs.d/$src"
        chown -R $user:$NINJAS_GROUP /home/$user/.emacs.d/$src
        chmod -R 770 /home/$user/.emacs.d/$src
    done
    
}

create_ninjas_emacsd

# EOF
