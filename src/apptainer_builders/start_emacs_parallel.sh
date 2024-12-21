#!/bin/bash
# Time-stamp: "2024-12-19 21:17:46 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/start_emacs_independently.sh

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/ENVS.sh.src
source "$THIS_DIR"/user_correct_permissions_emacsd.sh.src

xhost +local:root > /dev/null 2>&1

CRDT_PASSWORD=$(openssl rand -base64 12)
export CRDT_PASSWORD

init_environment() {
    killall -9 "$NINJA_EMACS_CLIENT" 2>/dev/null || true
    killall -9 "$NINJA_EMACS_BIN" 2>/dev/null || true
}

start_emacs_daemon() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id
    local log_file=/tmp/emacs-$NINJA_USER.log

    if [ -z "$DISPLAY" ]; then
        echo "Error: DISPLAY not set"
        return 1
    fi

    mkdir -p $(dirname $NINJA_EMACSD_SERVER_FILE)
    chown -R $NINJA_USER:$NINJA_USER $(dirname $NINJA_EMACSD_SERVER_FILE)
    chmod 700 $(dirname $NINJA_EMACSD_SERVER_FILE)
    rm -f $NINJA_EMACSD_SERVER_FILE >/dev/null
    rm -f $log_file 2>/dev/null

    # Start daemon and client in background
    (
        su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacs --daemon=$NINJA_EMACSD_SERVER_FILE -Q > $log_file 2>&1"
        sleep 1
        su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacsclient -s $NINJA_EMACSD_SERVER_FILE -c -n --eval '(load-file \"$NINJA_EMACSD_PRIVATE/init.el\")'" &
    ) &
}

main() {
    init_environment
    emacsd_correct_permissions
    
    # Start all daemons in parallel
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        start_emacs_daemon $ninja_id
    done

    # Wait for all background processes to complete
    wait
}

main
