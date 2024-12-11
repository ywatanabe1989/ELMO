#!/bin/bash
# Time-stamp: "2024-12-11 12:42:16 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_basic_apt_packages.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

apt-get -y update > /dev/null
apt-get install -y apt-utils > /dev/null

apt-get -y install \
    build-essential \
    git \
    wget \
    curl \
    passwd \
    sudo \
    adduser \
    w3m \
    openssh-client \
    gnupg \
    sqlite3 \
    locales \
    language-pack-en \
    parallel \
    > /dev/null

# LANG
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
locale-gen $LANG > /dev/null
update-locale LANG=$LANG LC_ALL=$LC_ALL > /dev/null


# EOF
