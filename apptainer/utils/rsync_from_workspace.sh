#!/bin/bash
# Time-stamp: "2024-12-22 22:39:03 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/LLEMACS/src/apptainer_builders/system_copy_from_workspace.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$APPNAME" != "LLEMACS" ]; then
    echo "This script must be run in the LLEMACS Apptainer Container"
    exit 1
fi

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

# Copy dotfiles to llemacss
LLEMACS_FROM_HOST_DIR="/workspace/private/from_host"
ls $LLEMACS_FROM_HOST_DIR

# Mapping
SRC_DIR=$LLEMACS_FROM_HOST_DIR/home

source /opt/llemacs/src/apptainer_builders/ENVS.sh.src

LLEMACS_FROM_HOST_SOURCES=(
    $SRC_DIR/.bashrc:$LLEMACS_HOME/.bashrc:
    $SRC_DIR/.bash_profile:$LLEMACS_HOME/.bash_profile
    $SRC_DIR/.bash.d/:$LLEMACS_HOME/.bash.d/
    $SRC_DIR/.ssh/:$LLEMACS_HOME/.ssh/
    $SRC_DIR/.pythonrc:$LLEMACS_HOME/.pythonrc
    $SRC_DIR/.pystartup:$LLEMACS_HOME/.pystartup
    $SRC_DIR/.bin/:$LLEMACS_HOME/.bin/
    $SRC_DIR/.gitconfig:$LLEMACS_HOME/.gitconfig
    $SRC_DIR/.git-templates:$LLEMACS_HOME/.git-templates/
    $SRC_DIR/.screenrc:$LLEMACS_HOME/.screenrc
)
# $SRC_DIR/.emacs.d/:$LLEMACS_HOME/.emacs.d/ # .emacs.d is copied from shared_emacsd

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
for i_llemacs in $(seq -f "%03g" 1 $LLEMACS_N_AGENTS); do
    update_llemacs_envs $i_llemacs
    echo $LLEMACS_HOME

    for mapping in "${LLEMACS_FROM_HOST_SOURCES[@]}"; do
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
# ssh-keygen -t ed25519 -f "${LLEMACS_HOME}/.ssh/id_ed25519" -N "" -C "llemacs-${i}"

# chown -R "llemacs-${i}:llemacs-${i}" "${LLEMACS_HOME}"
# chmod 700 "${LLEMACS_HOME}/.ssh"
# chmod 600 "${LLEMACS_HOME}/.ssh/"*


# EOF
