#!/bin/bash
# Time-stamp: "2024-12-24 14:38:44 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/llemacs/apptainer/llemacs.sandbox/opt/llemacs/apptainer/build/startup/start_emacs.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "$0..."

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/llemacs/config/env/00_all.env

# Allow X server access
xhost +local:root > /dev/null 2>&1

init_environment() {
    killall -9 emacsclient 2>/dev/null || true
    killall -9 emacs 2>/dev/null || true
}

start_emacs_daemon() {
    local log_file=$(mktemp /tmp/emacs-${ELMO_USER_USER}.XXXXXX)
    local llemacs_user=$ELMO_USER-$ELMO_ID
    local emacsd=$ELMO_HOME/.emacs.d
    # local emacsd=$ELMO_WORKSPACE/llemacss/$llemacs_user/.emacs.d
    local emacs_server_dir=$emacsd/emacs-server
    local emacs_server_file=$emacs_server_dir/server

    # Setup server directory
    mkdir -p $emacsd/emacs-server
    # chown $ELMO_USER:$ELMO_USER $emacs_server_dir
    chmod 700 $emacs_server_dir
    rm -f $emacs_server_dir/server* 2>/dev/null

    # # Kill
    pkill -f "emacs --daemon=$emacs_server_file"

    # Start server
    cmd_start_server="HOME=$ELMO_HOME \
        DISPLAY=$DISPLAY \
        LOGNAME=$llemacs_user \
        USER=$llemacs_user \
        emacs \
        --daemon=$emacs_server_file \
        --init-directory=$emacsd \
        -Q \
        2>&1"

    # Connect client
    cmd_connect_client="HOME=$ELMO_HOME \
        DISPLAY=$DISPLAY \
        LOGNAME=$llemacs_user \
        USER=$llemacs_user \
        emacsclient \
        -c \
        -n \
        -s $emacs_server_file \
        --eval '(load-file \"$emacsd/init.el\")' \
        >/dev/null &"

    echo $cmd_start_server && eval $cmd_start_server
    echo $cmd_connect_client && eval $cmd_connect_client
}

main() {
    init_environment
    start_emacs_daemon
}

main


# EOF
