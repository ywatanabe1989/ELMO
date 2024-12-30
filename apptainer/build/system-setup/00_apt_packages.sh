#!/bin/bash
# Time-stamp: "2024-12-24 11:41:07 (ywatanabe)"
# File: ./LLEMACS/src/apptainer_builders/install_basic_apt_packages.sh

echo "$0..."

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

export PATH="/usr/local/sbin:/usr/sbin:/sbin:$PATH"
source /opt/llemacs/config/env/00_all.env

# Initialization
apt-get -y update >/dev/null
apt-get install -y apt-utils >/dev/null

# # Before any package installations
# export DEBIAN_FRONTEND=noninteractive

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
        xclip \
        xsel \
        autossh \
        texlive-full \
        >/dev/null



# Language
locale-gen $LLEMACS_LANG >/dev/null
update-locale LANG=$LLEMACS_LANG
LC_ALL=$LLEMACS_LC_ALL >/dev/null


# EOF
