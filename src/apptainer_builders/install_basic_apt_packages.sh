#!/bin/bash
# Time-stamp: "2024-12-10 11:22:28 (ywatanabe)"
# File: ./ninja/src/apptainer_builders/install_basic_apt_packages.sh

export DEBIAN_FRONTEND=noninteractive

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
    sqlite3 \
    locales \
    language-pack-en

locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

# apt-get -y update && apt-get -y install \
#     build-essential \
#     git \
#     wget \
#     curl \
#     passwd \
#     sudo \
#     adduser \
#     w3m \
#     openssh-client \
#     gnupg \
#     sqlite3


# EOF
