#!/bin/bash
# Time-stamp: "2024-12-08 07:29:59 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/configure_git.sh

. "$(dirname $0)"/ENVS.sh.source

sudo -u $SEMACS_USER git config --global user.name "sea-bot"
sudo -u $SEMACS_USER git config --global user.email "sea-bot@example.com"
sudo -u $SEMACS_USER git config --global core.editor "$SEMACS_EMACS_BIN"
sudo -u $SEMACS_USER git config --global init.defaultBranch "main"

# EOF
