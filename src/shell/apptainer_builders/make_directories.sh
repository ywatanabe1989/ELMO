#!/bin/bash
# Time-stamp: "2024-12-08 07:52:25 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/make_directories.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

########################################
# Workspace directories
########################################
sudo -u $SEMACS_USER mkdir -p $SEMACS_WORKSPACE $SEMACS_BACKUPS $SEMACS_LOGS $SEMACS_REQUESTS $SEMACS_CONFIG

# EOF
