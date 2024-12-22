#!/bin/bash
# Time-stamp: "2024-12-23 01:42:04 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_basic_apt_packages.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

export PATH="/usr/local/sbin:/usr/sbin:/sbin:$PATH"
source /opt/Ninja/config/env/00_all.env

# Initialization
apt-get -y update >/dev/null
apt-get install -y apt-utils >/dev/null

# Before any package installations
export DEBIAN_FRONTEND=noninteractive

# Configure postfix to use 'No configuration' option
echo "postfix postfix/main_mailer_type select No configuration" | debconf-set-selections
echo "postfix postfix/mailname string localhost" | debconf-set-selections

# Remove and purge postfix completely
apt-get remove --purge -y postfix >/dev/null
apt-get autoremove -y >/dev/null
dpkg --configure -a >/dev/null

# Install packages without postfix
apt-get install -y --no-install-recommends \
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
        tree \
        ripgrep \
        auctex \
        dvipng \
        fonts-ipafont \
        fonts-ipaexfont \
        rsync \
        unzip \
        fontconfig \
        >/dev/null

# texlive-full \

# Language
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
locale-gen $LANG >/dev/null
update-locale LANG=$LANG LC_ALL=$LC_ALL >/dev/null


# EOF
