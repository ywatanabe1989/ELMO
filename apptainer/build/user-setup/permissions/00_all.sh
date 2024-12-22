#!/bin/bash
# Time-stamp: "2024-12-22 20:22:45 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/building/user-setup/permissions/00_all.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/01_emacsd.src
source /opt/Ninja/config/env/00_all.env

correct_permissions_home() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        update_ninja_envs "$ninja_id"
        chmod -R 770 "$NINJA_HOME"
        chown -R "$NINJA_USER:$NINJA_GROUP" "$NINJA_HOME"
    done
}

correct_permissions_global(){
    chown -R root:"$NINJA_GROUP" /home
    chown -R root:"$NINJA_GROUP" /opt/Ninja
    chmod -R 775 /opt/Ninja/src/apptainer_builders/shared_emacsd

    mkdir /workspace
    chmod -R 777 /workspace
    chown -R root:"$NINJA_GROUP" /workspace
}

correct_permissions_shared() {
    chmod -R 775 /opt/Ninja
    chown -R root:"$NINJA_GROUP" /opt/Ninja
}

correct_permissions_home
correct_permissions_shared
correct_permissions_global
emacsd_correct_permissions


# EOF


# EOF
