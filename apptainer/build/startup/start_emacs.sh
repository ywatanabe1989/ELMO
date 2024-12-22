#!/bin/bash
# Time-stamp: "2024-12-22 23:11:53 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/build/init/start_emacs.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/Ninja/config/env/00_all.env
source /opt/Ninja/apptainer/build/user-setup/permissions/01_emacsd.src

# Add at beginning of script
xhost +local:root > /dev/null 2>&1

# Generate and export CRDT password
CRDT_PASSWORD=$(openssl rand -base64 12)
export CRDT_PASSWORD

init_environment() {
    killall -9 emacsclient 2>/dev/null || true
    killall -9 emacs 2>/dev/null || true
}

start_emacs_daemon() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    local log_file=$(mktemp /tmp/emacs-${NINJA_USER}.XXXXXX)

    # Ensure directory exists and has correct permissions
    # mkdir -p $(dirname $NINJA_EMACSD_SERVER_FILE)
    # chown -R $NINJA_USER:$NINJA_USER $(dirname $NINJA_EMACSD_SERVER_FILE)
    # chmod 700 $(dirname $NINJA_EMACSD_SERVER_FILE)

    mkdir -p $NINJA_HOME/.emacs.d/emacs-server
    chown $NINJA_USER:$NINJA_USER $NINJA_HOME/.emacs.d/emacs-server
    chmod 700 $NINJA_USER:$NINJA_USER $NINJA_HOME/.emacs.d/emacs-server

    rm -f $NINJA_HOME/.emacs.d/emacs-server/server* 2>/dev/null

    # Server start
    su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacs --daemon=$NINJA_EMACSD_SERVER_FILE -Q 2>&1"

    # Connect
    su - $NINJA_USER -c \
       "HOME=$NINJA_HOME \
       DISPLAY=$DISPLAY \
       emacsclient -c -n -s $NINJA_EMACSD_SERVER_FILE \
       --eval '(load-file \"$NINJA_EMACSD_PRIVATE/init.el\")'"\
       >/dev/null &
}


main() {
    init_environment
    emacsd_correct_permissions
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        start_emacs_daemon $ninja_id
    done
}

main

# EOF


# EOF
