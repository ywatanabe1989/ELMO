#!/bin/bash
# Time-stamp: "2024-12-09 20:10:42 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

########################################
# Emacs
########################################
apt install -y \
        emacs \
        emacs-common \
        emacs-bin-common

sudo -u $NINJA_USER bash -c "rm -rf $NINJA_DOT_EMACS"
sudo -u $NINJA_USER bash -c "ln -s $NINJA_OPT_DIR/src/apptainer_builders/safe_dot_emacs.d $NINJA_HOME/"
sudo -u $NINJA_USER bash -c "ln -s $NINJA_OPT_DIR/src/apptainer_builders/start.sh $NINJA_HOME/"

# EOF
