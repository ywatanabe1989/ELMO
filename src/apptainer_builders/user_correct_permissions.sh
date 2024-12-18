#!/bin/bash
# Time-stamp: "2024-12-18 05:36:40 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_correct_permissions.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

correct_ninjas_permissions() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        correct_ninja_permissions $ninja_id
    done
}

correct_ninja_permissions() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # Ninja HOME: Readable by other ninjas
    chmod -R 750 $NINJA_HOME >/dev/null
    chown -R $NINJA_USER:$NINJAS_GROUP $NINJA_HOME >/dev/null

    # # Working Directories: Readable by other ninjas
    # for wdir in $NINJA_WORKSPACE $NINJA_BACKUPS $NINJA_LOGS $NINJA_REQUESTS $NINJA_CONFIG; do
    #     chmod -R 750 $wdir >/dev/null
    #     chown -R $NINJA_USER:$NINJAS_GROUP $wdir >/dev/null
    # done

    # Emacs
    
}

correct_global_permissions(){
    chown -R root:$NINJAS_GROUP /home
    chown -R root:$NINJAS_GROUP /opt/Ninja
    chmod -R 770 /opt/Ninja/src/apptainer_builders/shared_emacsd
}

correct_shared_permissions() {
    # Source
    chmod -R 750 -R /opt/Ninja
    chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null
}


correct_ninjas_permissions
correct_shared_permissions
correct_global_permissions

# EOF
