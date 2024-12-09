#!/bin/bash
# Time-stamp: "2024-12-08 07:49:06 (ywatanabe)"
# File: ./Ninja/src/shell/apptainer_builders/create_bashrc.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

sudo -u $NINJA_USER bash -c "echo 'export PATH=\$PATH:/usr/local/bin' >> $NINJA_HOME/.bashrc"
sudo -u $NINJA_USER bash -c "echo 'export PYTHONPATH=$PYTHONPATH' >> $NINJA_HOME/.bashrc"

# EOF
