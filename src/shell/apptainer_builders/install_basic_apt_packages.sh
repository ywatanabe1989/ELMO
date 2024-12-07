#!/bin/bash
# Time-stamp: "2024-12-08 07:49:03 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/install_basic_apt_packages.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

apt-get update && apt-get install -y sudo

########################################
# Basic development tools
########################################
apt-get install -y \
    build-essential \
    git \
    wget \
    curl \
    passwd \
    adduser \
    w3m \
    openssh-client \
    gnupg \
    sqlite3


# EOF
