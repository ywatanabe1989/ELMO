#!/bin/bash
# Time-stamp: "2024-12-08 03:49:59 (ywatanabe)"
# File: ./Ninja/src/shell/sea_server.sh

# Check if script is run with sudo
if [ "$EUID" -ne 0 ]; then
    echo "Please run as root (with sudo)"
    exit 1
fi

# Help message
show_help() {
    cat << EOF
SEA Server Control Script

Usage:
    $0 [command] [options]
    $0 execute "ELISP_CODE"    Execute elisp code in the server
                              Example: $0 execute '(message "hello")'

Commands:
    start       Start or connect to SEA server (default)
    kill        Kill the SEA server
    init     Init the SEA server
    status      Check server status
    execute     Execute elisp command
    help        Show this help message

Options:
    -u USER     SEA user (default: $NINJA_USER)
    -s SOCKET   Socket name (default: $NINJA_SOCKET_NAME)
    -h          Show this help message
EOF
    exit 0
}

# Argument parser
while getopts "u:s:h" opt; do
    case $opt in
        u) NINJA_USER="$OPTARG" ;;
        s) NINJA_SOCKET_NAME="$OPTARG" ;;
        h) show_help ;;
    esac
done

shift $((OPTIND-1))
COMMAND="${1:-start}"

ninja_kill_server() {
    if _ninja_is_server_running; then
        sudo -u "$NINJA_USER" emacsclient -e '(kill-emacs)' && sleep 1
        sudo rm "$NINJA_SOCKET_FILE"
        if _ninja_is_server_running; then
            sudo pkill -u "$NINJA_USER" && sleep 1
        fi
    fi
}

ninja_init_server() {
    ninja_kill_server
    _ninja_setup_server_dir

    # Start daemon
    sudo -u "$NINJA_USER" \
         HOME="$NINJA_DOT_EMACS" \
         emacs \
         --daemon="$NINJA_SOCKET_FILE" \
         --init-directory="$NINJA_DOT_EMACS" &

    sudo -u "$NINJA_USER" ls "$NINJA_SOCKET_FILE"
}

ninja_init_or_connect() {
    local connected=0
    if ! _ninja_is_server_running; then
        ninja_init_server
        sleep 1
    fi

    _ninja_connect_server

}

ninja_eval_elisp() {
    if _ninja_is_server_running; then
        sudo -u "$NINJA_USER" HOME="$NINJA_DOT_EMACS" emacsclient -s "$NINJA_SOCKET_FILE" -e "$1"
    else
        echo "Server is not running"
        exit 1
    fi
}

_ninja_is_server_running() {
    # Check process exists
    if ! pgrep -u "$NINJA_USER" emacs >/dev/null; then
        return 1
    fi

    # Check server is accepting connections
    if ! sudo -u "$NINJA_USER" \
         HOME="$NINJA_DOT_EMACS" \
         emacsclient -s \
         "$NINJA_SOCKET_FILE" \
         -e '(version)' \
         >/dev/null 2>&1; then
        return 1
    fi

    return 0
}

_ninja_setup_server_dir() {
    sudo rm -rf "$NINJA_SOCKET_DIR"
    sudo -u "$NINJA_USER" mkdir -p "$NINJA_SOCKET_DIR"
    sudo chmod 700 "$NINJA_SOCKET_DIR"
    sudo chown "$NINJA_USER":"$NINJA_USER" "$NINJA_SOCKET_DIR"
    # sudo chmod 770 "$NINJA_SOCKET_DIR"
    # sudo chmod 770 "$NINJA_SOCKET_FILE"
    sudo chown "$NINJA_USER":"$NINJA_USER" "$NINJA_SOCKET_DIR"
}

# _ninja_connect_server() {
#     sudo -u "$NINJA_USER" HOME="$NINJA_HOME" emacsclient -s "$NINJA_SOCKET_FILE" -c &
# }

_ninja_connect_server() {
    sudo -u "$NINJA_USER" HOME="$NINJA_DOT_EMACS" emacsclient -s "$NINJA_SOCKET_FILE" -c &
}

case "$COMMAND" in
    start)   ninja_init_or_connect & ;;
    kill)    ninja_kill_server & ;;
    init) ninja_kill_server && ninja_init_or_connect & ;;
    status)  _ninja_is_server_running ;;
    execute) ninja_eval_elisp "$2" & ;;
    help)    show_help ;;
    *)       show_help ;;
esac


# EOF
