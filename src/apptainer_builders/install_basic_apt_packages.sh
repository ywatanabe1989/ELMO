#!/bin/bash
# Time-stamp: "2024-12-09 20:23:01 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_basic_apt_packages.sh

apt-get -y update && apt-get -y install \
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
    sqlite3


# EOF
