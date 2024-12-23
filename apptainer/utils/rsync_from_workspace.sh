#!/bin/bash
# Time-stamp: "2024-12-22 22:39:03 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/src/apptainer_builders/system_copy_from_workspace.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$APPNAME" != "Ninja" ]; then
    echo "This script must be run in the Ninja Apptainer Container"
    exit 1
fi

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

# Copy dotfiles to ninjas
NINJA_FROM_HOST_DIR="/workspace/private/from_host"
ls $NINJA_FROM_HOST_DIR

# Mapping
SRC_DIR=$NINJA_FROM_HOST_DIR/home

source /opt/Ninja/src/apptainer_builders/ENVS.sh.src

NINJA_FROM_HOST_SOURCES=(
    $SRC_DIR/.bashrc:$NINJA_HOME/.bashrc:
    $SRC_DIR/.bash_profile:$NINJA_HOME/.bash_profile
    $SRC_DIR/.bash.d/:$NINJA_HOME/.bash.d/
    $SRC_DIR/.ssh/:$NINJA_HOME/.ssh/
    $SRC_DIR/.pythonrc:$NINJA_HOME/.pythonrc
    $SRC_DIR/.pystartup:$NINJA_HOME/.pystartup
    $SRC_DIR/.bin/:$NINJA_HOME/.bin/
    $SRC_DIR/.gitconfig:$NINJA_HOME/.gitconfig
    $SRC_DIR/.git-templates:$NINJA_HOME/.git-templates/
    $SRC_DIR/.screenrc:$NINJA_HOME/.screenrc
)
# $SRC_DIR/.emacs.d/:$NINJA_HOME/.emacs.d/ # .emacs.d is copied from shared_emacsd

# Copy from host to workspace
RSYNC_OPTIONS="-av \
    --safe-links \
    --exclude=**/.git \
    --exclude=**/*.sandbox \
    --exclude=**/*.sif \
    --exclude=**/__pycache__ \
    --exclude=**/*docker* \
    --exclude=**/.apptainer \
    --exclude=**/.old \
    --exclude=**/.* \
    --exclude=**/*cache* \
    --exclude=**/build-temp* \
    --exclude=**/.control \
    --exclude=**/RUNNING \
    --exclude=**/FINISHED
    "

# Deploy
for i_ninja in $(seq -f "%03g" 1 $NINJA_N_AGENTS); do
    update_ninja_envs $i_ninja
    echo $NINJA_HOME

    for mapping in "${NINJA_FROM_HOST_SOURCES[@]}"; do
        src="${mapping%%:*}"
        dst="${mapping#*:}"

        echo "$(dirname $dst)"
        echo ""
        echo $src
        echo $dst
        echo ""

        mkdir -p "$(dirname $dst)"
        rsync -aLn $RSYNC_OPTIONS $src $dst
    done

    echo $dst
done

# # SSH setup
# ssh-keygen -t ed25519 -f "${NINJA_HOME}/.ssh/id_ed25519" -N "" -C "ninja-${i}"

# chown -R "ninja-${i}:ninja-${i}" "${NINJA_HOME}"
# chmod 700 "${NINJA_HOME}/.ssh"
# chmod 600 "${NINJA_HOME}/.ssh/"*


# EOF
