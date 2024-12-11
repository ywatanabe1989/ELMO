#!/bin/bash
# Time-stamp: "2024-12-11 13:44:49 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_create_bashrc.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

ENVS_SRC="$(dirname $0)"/ENVS.sh.src
source $ENVS_SRC

create_ubuntu_user_bashrc() {
    echo "" >> $NINJA_HOME/.bashrc
    echo "========================================" >> $NINJA_HOME/.bashrc
    echo "" >> $NINJA_HOME/.bashrc
    cat $ENVS_SRC >> $NINJA_HOME/.bashrc
}

create_ninjas_bashrc() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_bashrc $ninja_id
    done
}

create_ninja_bashrc() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    echo "" >> $NINJA_HOME/.bashrc
    echo "========================================" >> $NINJA_HOME/.bashrc
    echo "" >> $NINJA_HOME/.bashrc
    cat $ENVS_SRC >> $NINJA_HOME/.bashrc
}

create_ninjas_bashrc


# EOF
