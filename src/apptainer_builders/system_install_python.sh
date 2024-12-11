#!/bin/bash
# Time-stamp: "2024-12-11 11:08:59 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_python.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

apt install -y \
    python3 \
    python3-pip \
    python3-venv \
    >/dev/null

ln -s /bin/python3 /bin/python
ln -s /usr/bin/python3 /usr/bin/python

# EOF
