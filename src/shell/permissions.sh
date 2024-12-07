#!/bin/bash
# Time-stamp: "2024-12-08 03:57:25 (ywatanabe)"
# File: ./Semacs/src/shell/permissions.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script must be run as root" >&2
   exit 1
fi

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
[ -S "$SEMACS_SOCKET_FILE" ] && { chmod 660 "$SEMACS_SOCKET_FILE" || exit 1; }
[ -S "$SEMACS_SOCKET_FILE" ] && { chgrp $SEMACS_GROUP "$SEMACS_SOCKET_FILE" || exit 1; }

# EOF
