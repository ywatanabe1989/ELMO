#!/bin/bash
# Time-stamp: "2024-12-23 11:08:38 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/build/init/start_emacs.sh

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/elmo/config/env/00_all.env
source /opt/elmo/apptainer/build/user-setup/permissions/01_emacsd.src

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
    local elmo_id="$1"
    update_elmo_envs $elmo_id

    local log_file=$(mktemp /tmp/emacs-${ELMO_USER}.XXXXXX)

    # Ensure directory exists and has correct permissions
    # mkdir -p $(dirname $ELMO_EMACSD_SERVER_FILE)
    # chown -R $ELMO_USER:$ELMO_USER $(dirname $ELMO_EMACSD_SERVER_FILE)
    # chmod 700 $(dirname $ELMO_EMACSD_SERVER_FILE)

    mkdir -p $ELMO_HOME/.emacs.d/emacs-server
    chown $ELMO_USER:$ELMO_USER $ELMO_HOME/.emacs.d/emacs-server
    chmod 700 $ELMO_USER:$ELMO_USER $ELMO_HOME/.emacs.d/emacs-server

    rm -f $ELMO_HOME/.emacs.d/emacs-server/server* 2>/dev/null

    # Server start
    su - $ELMO_USER -c "HOME=$ELMO_HOME DISPLAY=$DISPLAY emacs --daemon=$ELMO_EMACSD_SERVER_FILE -Q 2>&1"

    # Connect
    su - $ELMO_USER -c \
       "HOME=$ELMO_HOME \
       DISPLAY=$DISPLAY \
       emacsclient -c -n -s $ELMO_EMACSD_SERVER_FILE \
       --eval '(load-file \"$ELMO_EMACSD_PRIVATE/init.el\")'"\
       >/dev/null &
}


main() {
    init_environment
    emacsd_correct_permissions
    for elmo_id in $(seq 1 $ELMO_N_AGENTS); do
        start_emacs_daemon $elmo_id
    done
}

main

# EOF


# EOF
