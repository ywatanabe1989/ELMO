#!/bin/bash
# Time-stamp: "2024-12-17 13:07:00 (ywatanabe)"
# File: ./Ninja/.apptainer/ninja/ninja.sandbox/opt/Ninja/src/apptainer_builders/start_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

rm_socket() {
    if [ -d "$NINJA_EMACS_SERVER_SOCKET_DIR" ]; then
        rm -rf "$NINJA_EMACS_SERVER_SOCKET_DIR"
    fi
}

correct_socket_dir_permissions() {

    mkdir -p "$NINJA_EMACS_SERVER_SOCKET_DIR"
    chown root:"$NINJAS_GROUP" "$NINJA_EMACS_SERVER_SOCKET_DIR"
    chmod 770 "$NINJA_EMACS_SERVER_SOCKET_DIR"
    if [ $? -ne 0 ]; then
        echo "Error setting permissions for $NINJA_EMACS_SERVER_SOCKET_DIR" >&2
        exit 1
    fi

    echo ""
    echo "----------------------------------------"
    echo "Socket directory permissions were corrected."
    echo "$NINJA_EMACS_SERVER_SOCKET_DIR"
    ls "$NINJA_EMACS_SERVER_SOCKET_DIR" -al
    echo "----------------------------------------"
    echo ""
}


correct_socket_permissions() {
    local socket_dir
    socket_dir=$(dirname "$NINJA_EMACS_SERVER_SOCKET")
    if [ -d "$socket_dir" ]; then
        chown -R root:"$NINJAS_GROUP" "$socket_dir"
        chmod -R 770 "$socket_dir"
        if [ $? -ne 0 ]; then
            echo "Error setting permissions for $socket_dir" >&2
            exit 1
        fi
    fi
    echo ""
    echo "----------------------------------------"
    echo "Socket permissions were corrected."
    echo "$NINJA_EMACS_SERVER_SOCKET"
    ls "$NINJA_EMACS_SERVER_SOCKET" -al
    echo "----------------------------------------"
    echo ""
}

debugging_echo() {
    # User
    echo "User: $NINJA_USER"

    # Socket directory
    echo "Socket Directory: $NINJA_EMACS_SERVER_SOCKET_DIR"
    ls "$NINJA_EMACS_SERVER_SOCKET_DIR" -al

    # Socket
    echo "Socket: $NINJA_EMACS_SERVER_SOCKET"
    if [ -S "$NINJA_EMACS_SERVER_SOCKET" ]; then
        ls "$NINJA_EMACS_SERVER_SOCKET" -al
    fi
}

display_check() {
    # Display
    echo "Display: $DISPLAY"
    if [ -z "$DISPLAY" ]; then
        echo "DISPLAY variable is not set. Cannot start GUI emacs."
        return 1
    fi
}

get_connected_clients() {
    local count
    count=$("$NINJA_EMACS_CLIENT" -s "$NINJA_EMACS_SERVER_SOCKET" --eval '(message (length server-clients))')

    echo ""
    echo "========================================"
    echo "Number of connected clients: $count"
    echo "========================================"
    echo ""
}

connect_user() {
    local current_user="$1"
    local current_display="$2"
    su - $current_user -c "DISPLAY=$current_display $NINJA_EMACS_CLIENT -c -s $NINJA_EMACS_SERVER_SOCKET &"
    get_connected_clients
}


kill_emacs() {
    echo "pkill -9 -f \"$NINJA_EMACS_CLIENT.*$NINJA_EMACS_SERVER_SOCKET\""
    pkill -9 -f "$NINJA_EMACS_CLIENT.*$NINJA_EMACS_SERVER_SOCKET"

}

connect_gui() {
    DISPLAY="$DISPLAY" "$NINJA_EMACS_CLIENT" -s "$NINJA_EMACS_SERVER_SOCKET" -c &
}

start_emacs() {

    rm_socket
    correct_socket_dir_permissions
    display_check

    # Start the emacs server before the loop
    rm -f "$NINJA_EMACS_SERVER_SOCKET"
    kill_emacs
    echo "----------------------------------------"
    echo $HOME
    echo "----------------------------------------"

    echo aaa
    # # does not work due to .emacs.d
    # DISPLAY=$DISPLAY \
        #     $NINJA_EMACS_BIN \
        #     --daemon=$NINJA_EMACS_SERVER_SOCKET &
    # Wrong type argument: frame-live-p, #<dead frame F2 Ox557eeba308fO>

    DISPLAY=$DISPLAY \
        $NINJA_EMACS_BIN \
        --daemon=$NINJA_EMACS_SERVER_SOCKET \
        --init-directory=/opt/Ninja/src/apptainer_builders/shared_emacsd/ &

    # Wait for socket creation
    while [ ! -S "$NINJA_EMACS_SERVER_SOCKET" ]; do
        echo "Waiting for socket..."
        sleep 1
    done

    # # Working without loading init.el
    # $NINJA_EMACS_BIN --daemon=$NINJA_EMACS_SERVER_SOCKET
    correct_socket_dir_permissions
    correct_socket_permissions

    connect_user ninja-001 :1
    connect_user ninja-002 :1
    connect_user ninja-003 :1
    connect_user ninja-004 :1

    # I would like to load host user's ~/.emacs.d
    # DISPLAY=$DISPLAY $NINJA_EMACS_CLIENT /tmp/dev.el -s $NINJA_EMACS_SERVER_SOCKET -c &
    connect_gui

    sleep 120
    # $NINJA_EMACS_CLIENT -s $NINJA_EMACS_SERVER_SOCKET --eval '(length server-clients)'
}

start_emacs

echo "
apptainer run \
          --fakeroot \
          --writable \
          ./.apptainer/ninja/ninja.sandbox
/opt/Ninja/src/apptainer_builders/start_emacs.sh
"

# EOF
