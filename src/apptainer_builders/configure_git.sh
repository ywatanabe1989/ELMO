#!/bin/bash
# Time-stamp: "2024-12-08 07:29:59 (ywatanabe)"
# File: ./Ninja/src/shell/apptainer_builders/configure_git.sh

. "$(dirname $0)"/ENVS.sh.source

sudo -u $NINJA_USER git config --global user.name "sea-bot"
sudo -u $NINJA_USER git config --global user.email "sea-bot@example.com"
sudo -u $NINJA_USER git config --global core.editor "$NINJA_EMACS_BIN"
sudo -u $NINJA_USER git config --global init.defaultBranch "main"

# EOF
