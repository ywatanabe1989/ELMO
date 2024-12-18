#!/bin/bash
# Time-stamp: "2024-12-18 21:39:24 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/start_emacs_independently.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source "$(dirname $0)"/ENVS.sh.src
source "$(dirname $0)"/user_correct_permissions_emacsd.sh.src

# Add at beginning of script
xhost +local:root > /dev/null 2>&1

# Generate and export CRDT password
CRDT_PASSWORD=$(openssl rand -base64 12)
export CRDT_PASSWORD

init_environment() {
    killall -9 "$NINJA_EMACS_CLIENT" 2>/dev/null || true
    killall -9 "$NINJA_EMACS_BIN" 2>/dev/null || true
}

# start_emacs_daemon() {
#     local user=$1
#     local display=$2
#     local user_socket_dir="/home/$user/emacs-server"
#     local user_socket="$user_socket_dir/server"
#     local log_file=/tmp/emacs-$user.log

#     mkdir -p $user_socket_dir
#     chown $user:$user $user_socket_dir
#     chmod 700 $user_socket_dir

#     # Cleanup existing socket
#     rm -f $user_socket 2>&1 >/dev/null

#     # Start daemon
#     rm $log_file 2>&1 >/dev/null
#     su - $user -c "DISPLAY=$DISPLAY emacs --daemon=$user_socket -Q > $log_file 2>&1" &

#     # Verify daemon
#     sleep 2
#     if ! pgrep -u $user emacs >/dev/null; then
#         echo "Emacs daemon failed to start for $user"
#         cat $log_file
#         return 1
#     fi

#     # Wait for socket
#     local attempt=0
#     while [ ! -S "$user_socket" ] && [ $attempt -lt 10 ]; do
#         echo "Waiting for socket ($attempt/10)..."
#         ls -l "$user_socket"* 2>/dev/null || true
#         sleep 1
#         ((attempt++))
#     done


#     # Start client with init file
#     if su - $user -c "DISPLAY=$DISPLAY emacsclient -c -n -s $user_socket --eval '(load-file \"~/.emacs.d/init.el\")' > /dev/null 2>&1 &"; then
#         echo "Success: $user_socket launched"
#         return 0
#     else
#         echo "Failed: Check error log below"
#         cat $log_file
#         return 1
#     fi

# }


# start_emacs_for_user() {
#     local user=$1
#     local display=$2

#     # setup_user_directories "$user"
#     start_emacs_daemon "$user" "$DISPLAY"
# }

start_emacs_daemon() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id
    local log_file=/tmp/emacs-$user.log

    # mkdir -p $NINJA_EMACSD_SERVER_DIR
    # chown $NINJA_USER:$NINJA_USER $NINJA_EMACSD_SERVER_DIR
    # chmod 700 $NINJA_EMACSD_SERVER_DIR

    # Cleanup existing socket
    rm -f $NINJA_USER_socket 2>&1 >/dev/null

    # Start daemon
    rm $log_file 2>&1 >/dev/null
    su - $NINJA_USER -c "DISPLAY=$DISPLAY emacs --daemon=$NINJA_USER_socket -Q > $log_file 2>&1" &

    # Verify daemon
    sleep 2
    if ! pgrep -u $NINJA_USER emacs >/dev/null; then
        echo "Emacs daemon failed to start for $NINJA_USER"
        cat $log_file
        return 1
    fi

    # Wait for socket
    local attempt=0
    while [ ! -S "$NINJA_USER_socket" ] && [ $attempt -lt 10 ]; do
        echo "Waiting for socket ($attempt/10)..."
        ls -l "$NINJA_USER_socket"* 2>/dev/null || true
        sleep 1
        ((attempt++))
    done


    # Start client with init file
    if su - $NINJA_USER -c "DISPLAY=$DISPLAY emacsclient -c -n -s $NINJA_USER_socket --eval '(load-file \"~/.emacs.d/init.el\")' > /dev/null 2>&1 &"; then
        echo "Success: $NINJA_USER_socket launched"
        return 0
    else
        echo "Failed: Check error log below"
        cat $log_file
        return 1
    fi

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


