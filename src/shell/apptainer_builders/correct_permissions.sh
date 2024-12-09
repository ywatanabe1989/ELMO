#!/bin/bash
# Time-stamp: "2024-12-08 07:51:02 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/correct_permissions.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi


# ########################################
# # sudo permissions
# ########################################
# chown root:root /usr/bin/sudo
# chmod 4755 /usr/bin/sudo
# chown root:root /etc/sudo.conf
# chmod 644 /etc/sudo.conf

# chmod 774 /opt
# chmod 774 -R /opt/self-evolving-agent
# chown $SEMACS_USER:$SEMACS_USER -R /opt/self-evolving-agent

# Users
getent group $SEMACS_GROUP || groupadd $SEMACS_GROUP
usermod -a -G $SEMACS_GROUP $USER || exit 1
usermod -a -G $SEMACS_GROUP $SEMACS_USER || exit 1

# Set semacs home dir
chmod -R 770 $SEMACS_HOME || exit 1
chown -R $SEMACS_USER:$SEMACS_GROUP $SEMACS_HOME || exit 1

# Set socket directory permissions
mkdir -p $SEMACS_SERVER_SOCKET_DIR || exit 1
chmod 2775 $SEMACS_SERVER_SOCKET_DIR || exit 1
chgrp $SEMACS_GROUP $SEMACS_SERVER_SOCKET_DIR || exit 1

# Set socket file permissions (if exists)
[ -S "$SEMACS_SERVER_SOCKET" ] && { chmod 660 "$SEMACS_SERVER_SOCKET" || exit 1; }
[ -S "$SEMACS_SERVER_SOCKET" ] && { chgrp $SEMACS_GROUP "$SEMACS_SERVER_SOCKET" || exit 1; }

# EOF
