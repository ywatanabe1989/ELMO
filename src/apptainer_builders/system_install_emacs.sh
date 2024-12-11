#!/bin/bash
# Time-stamp: "2024-12-11 11:07:23 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

apt install -y \
        emacs \
        emacs-common \
        emacs-bin-common \
        >/dev/null

# EOF
