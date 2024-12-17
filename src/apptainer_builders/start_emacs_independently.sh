#!/bin/bash
# Time-stamp: "2024-12-17 16:09:24 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/start_emacs_independently.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source "$(dirname $0)"/ENVS.sh.src


# Add at beginning of script
xhost +local:root > /dev/null 2>&1

init() {
    # Kill existing emacs processes for all users
    killall -9 "$NINJA_EMACS_CLIENT" 2>/dev/null || true
    killall -9 "$NINJA_EMACS_BIN" 2>/dev/null || true
}

start_emacs_for_user() {
    user=$1
    display=$2
    user_socket_dir="/home/$user"
    user_socket="$user_socket_dir/server"
    shared_emacsd=/opt/Ninja/src/apptainer_builders/shared_emacsd

    
    # Permissions
    chown root:$NINJAS_GROUP -R /home
    chown root:$NINJAS_GROUP -R $shared_emacsd
    chmod 777 -R $shared_emacsd

    # Cleanup
    rm -f $user_socket
    su - $user -c "$NINJA_EMACS_CLIENT --eval '(kill-emacs)'" 2>/dev/null

    # Socket Directory
    # Creation
    mkdir -p $user_socket_dir
    chown $user:$NINJAS_GROUP $user_socket_dir
    chmod 770 $user_socket_dir

    # Link shared .emacs.d
    su - $user -c "rm -rf /home/$user/.emacs.d"
    su - $user -c "ln -sf $shared_emacsd /home/$user/.emacs.d"

    # Start server with explicit X11 forwarding
    su - $user -c "DISPLAY=$DISPLAY $NINJA_EMACS_BIN --daemon=$user_socket --init-directory=/home/$user/.emacs.d &"
    sleep 2

    while [ ! -S "$user_socket" ]; do
        echo "waiting"
        sleep 1
    done

    echo ""
    echo "----------------------------------------"
    echo "$user_socket was prepared"
    echo "----------------------------------------"
    echo ""

    # Launch client
    # su - $user -c "DISPLAY=$display XAUTHORITY=/root/.Xauthority $NINJA_EMACS_CLIENT -c -s $user_socket"
    # su - $user -c "DISPLAY=$display $NINJA_EMACS_CLIENT -c -s $user_socket --eval '(x-focus-frame nil)' &"
    # # su - $user -c "DISPLAY=$display $NINJA_EMACS_CLIENT -c -s $user_socket -n -a ''" &
    su - $user -c "DISPLAY=$display $NINJA_EMACS_CLIENT -c -n -s $user_socket" &
}

init
start_emacs_for_user ninja-001 :0
start_emacs_for_user ninja-002 :0
start_emacs_for_user ninja-003 :0
start_emacs_for_user ninja-004 :0


# /opt/Ninja/src/apptainer_builders/start_emacs_independently.sh
# EOF
