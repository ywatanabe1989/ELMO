#!/bin/bash
# Time-stamp: "2024-12-22 22:39:03 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/src/apptainer_builders/system_copy_from_workspace.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$APPNAME" != "ELMO" ]; then
    echo "This script must be run in the ELMO Apptainer Container"
    exit 1
fi

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

# Copy dotfiles to elmos
ELMO_FROM_HOST_DIR="/workspace/private/from_host"
ls $ELMO_FROM_HOST_DIR

# Mapping
SRC_DIR=$ELMO_FROM_HOST_DIR/home

source /opt/ELMO/src/apptainer_builders/ENVS.sh.src

ELMO_FROM_HOST_SOURCES=(
    $SRC_DIR/.bashrc:$ELMO_HOME/.bashrc:
    $SRC_DIR/.bash_profile:$ELMO_HOME/.bash_profile
    $SRC_DIR/.bash.d/:$ELMO_HOME/.bash.d/
    $SRC_DIR/.ssh/:$ELMO_HOME/.ssh/
    $SRC_DIR/.pythonrc:$ELMO_HOME/.pythonrc
    $SRC_DIR/.pystartup:$ELMO_HOME/.pystartup
    $SRC_DIR/.bin/:$ELMO_HOME/.bin/
    $SRC_DIR/.gitconfig:$ELMO_HOME/.gitconfig
    $SRC_DIR/.git-templates:$ELMO_HOME/.git-templates/
    $SRC_DIR/.screenrc:$ELMO_HOME/.screenrc
)
# $SRC_DIR/.emacs.d/:$ELMO_HOME/.emacs.d/ # .emacs.d is copied from shared_emacsd

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
for i_elmo in $(seq -f "%03g" 1 $ELMO_N_AGENTS); do
    update_elmo_envs $i_elmo
    echo $ELMO_HOME

    for mapping in "${ELMO_FROM_HOST_SOURCES[@]}"; do
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
# ssh-keygen -t ed25519 -f "${ELMO_HOME}/.ssh/id_ed25519" -N "" -C "elmo-${i}"

# chown -R "elmo-${i}:elmo-${i}" "${ELMO_HOME}"
# chmod 700 "${ELMO_HOME}/.ssh"
# chmod 600 "${ELMO_HOME}/.ssh/"*


# EOF
