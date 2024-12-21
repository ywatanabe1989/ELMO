#!/bin/bash
# Time-stamp: "2024-12-19 21:24:30 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/start_emacs_independently.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/ENVS.sh.src
source "$THIS_DIR"/user_correct_permissions_emacsd.sh.src

# Add at beginning of script
xhost +local:root > /dev/null 2>&1

# Generate and export CRDT password
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

    # Ensure directory exists and has correct permissions
    mkdir -p $(dirname $NINJA_EMACSD_SERVER_FILE)
    chown -R $NINJA_USER:$NINJA_USER $(dirname $NINJA_EMACSD_SERVER_FILE)
    chmod 700 $(dirname $NINJA_EMACSD_SERVER_FILE)

    rm -f $NINJA_EMACSD_SERVER_FILE >/dev/null
    rm -f $log_file 2>/dev/null

    # Server start
    su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacs --daemon=$NINJA_EMACSD_SERVER_FILE -Q > $log_file 2>&1"

    # Connect
    su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacsclient -s $NINJA_EMACSD_SERVER_FILE -c -n --eval '(load-file \"$NINJA_EMACSD_PRIVATE/init.el\")'" >/dev/null &
    # su - $NINJA_USER -c "HOME=$NINJA_HOME DISPLAY=$DISPLAY emacsclient -s $NINJA_EMACSD_SERVER_FILE -c -n --eval '(progn (load-file \"$NINJA_EMACSD_PRIVATE/init.el\") nil)'" >/dev/null &

    
}


main() {
    init_environment
    emacsd_correct_permissions
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        start_emacs_daemon $ninja_id
    done

    # sleep 120
}

main

# EOF
