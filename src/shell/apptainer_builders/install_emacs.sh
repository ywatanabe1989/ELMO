#!/bin/bash
# Time-stamp: "2024-12-08 07:49:05 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.source

########################################
# Emacs
########################################
apt-get install -y \
        emacs \
        emacs-common \
        emacs-bin-common

sudo -u $SEMACS_USER bash -c "rm -rf $SEMACS_DOT_EMACS"
sudo -u $SEMACS_USER bash -c "ln -s $SEMACS_OPT_DIR/src/shell/apptainer_builders/safe_dot_emacs.d $SEMACS_HOME/"
sudo -u $SEMACS_USER bash -c "ln -s $SEMACS_OPT_DIR/src/shell/apptainer_builders/start.sh $SEMACS_HOME/"

# EOF
