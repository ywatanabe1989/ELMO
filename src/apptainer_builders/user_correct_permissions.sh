#!/bin/bash
# Time-stamp: "2024-12-11 14:03:12 (ywatanabe)"
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

    # Working Directories: Readable by other ninjas
    for wdir in $NINJA_WORKSPACE $NINJA_BACKUPS $NINJA_LOGS $NINJA_REQUESTS $NINJA_CONFIG; do
        chmod -R 750 $wdir >/dev/null
        chown -R $NINJA_USER:$NINJAS_GROUP $wdir >/dev/null
    done
    }

correct_shared_permissions() {
    # Source
    chmod -R 750 -R /opt/Ninja
    chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null
}

correct_ninjas_permissions
correct_shared_permissions


# # Set socket directory permissions
# mkdir -p $NINJA_SERVER_SOCKET_DIR || exit 1
# chmod 2775 $NINJA_SERVER_SOCKET_DIR || exit 1
# chgrp $NINJA_GROUP $NINJA_SERVER_SOCKET_DIR || exit 1

# # Set socket file permissions (if exists)
# [ -S "$NINJA_SERVER_SOCKET" ] && { chmod 660 "$NINJA_SERVER_SOCKET" || exit 1; }
# [ -S "$NINJA_SERVER_SOCKET" ] && { chgrp $NINJA_GROUP "$NINJA_SERVER_SOCKET" || exit 1; }

# ########################################
# # sudo permissions
# ########################################
# chown root:root /usr/bin/sudo
# chmod 4755 /usr/bin/sudo
# chown root:root /etc/sudo.conf
# chmod 644 /etc/sudo.conf

# chmod 774 /opt
# chmod 774 -R /opt/self-evolving-agent
# chown $NINJA_USER:$NINJA_USER -R /opt/self-evolving-agent

# EOF
