#!/bin/bash
# Time-stamp: "2024-12-19 21:14:19 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/user_correct_permissions.sh

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/ENVS.sh.src
source "$THIS_DIR"/user_correct_permissions_emacsd.sh.src

correct_permissions_home() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        update_ninja_envs "$ninja_id"
        chmod -R 770 "$NINJA_HOME"
        chown -R "$NINJA_USER:$NINJAS_GROUP" "$NINJA_HOME"
    done
}

correct_permissions_global(){
    chown -R root:"$NINJAS_GROUP" /home
    chown -R root:"$NINJAS_GROUP" /opt/Ninja
    chmod -R 775 /opt/Ninja/src/apptainer_builders/shared_emacsd

    mkdir /workspace
    chmod -R 777 /workspace
    chown -R root:"$NINJAS_GROUP" /workspace
}

correct_permissions_shared() {
    chmod -R 775 /opt/Ninja
    chown -R root:"$NINJAS_GROUP" /opt/Ninja
}

correct_permissions_home
correct_permissions_shared
correct_permissions_global
emacsd_correct_permissions


# EOF
