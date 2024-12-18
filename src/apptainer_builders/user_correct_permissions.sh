#!/bin/bash
# Time-stamp: "2024-12-19 01:04:39 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_correct_permissions.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/ENVS.sh.src
source "$THIS_DIR"/user_correct_permissions_emacsd.sh.src

correct_permissions_home() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        update_ninja_envs $ninja_id

        chmod -R 750 $NINJA_HOME >/dev/null
        chown -R $NINJA_USER:$NINJAS_GROUP $NINJA_HOME >/dev/null        
    done
}

correct_permissions_global(){
    chown -R root:$NINJAS_GROUP /home
    chown -R root:$NINJAS_GROUP /opt/Ninja
    chmod -R 770 /opt/Ninja/src/apptainer_builders/shared_emacsd
}

correct_permissions_shared() {
    # Source
    chmod -R 750 -R /opt/Ninja
    chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null
}

correct_permissions_home
correct_permissions_shared
correct_permissions_global
# correct_permissions_emacsd
emacsd_correct_permissions

# EOF
