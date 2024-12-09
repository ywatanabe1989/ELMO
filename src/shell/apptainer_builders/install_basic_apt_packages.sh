#!/bin/bash
# Time-stamp: "2024-12-09 10:54:04 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/install_basic_apt_packages.sh

apt-get -y update && apt-get -y install \
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
