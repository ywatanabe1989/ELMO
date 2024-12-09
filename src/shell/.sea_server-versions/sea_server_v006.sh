#!/bin/bash
# Time-stamp: "2024-12-08 03:49:59 (ywatanabe)"
# File: ./Semacs/src/shell/sea_server.sh

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
    -u USER     SEA user (default: $SEMACS_USER)
    -s SOCKET   Socket name (default: $SEMACS_SOCKET_NAME)
    -h          Show this help message
EOF
    exit 0
}

# Argument parser
while getopts "u:s:h" opt; do
    case $opt in
        u) SEMACS_USER="$OPTARG" ;;
        s) SEMACS_SOCKET_NAME="$OPTARG" ;;
        h) show_help ;;
    esac
done

shift $((OPTIND-1))
COMMAND="${1:-start}"

semacs_kill_server() {
    if _semacs_is_server_running; then
        sudo -u "$SEMACS_USER" emacsclient -e '(kill-emacs)' && sleep 1
        sudo rm "$SEMACS_SOCKET_FILE"
        if _semacs_is_server_running; then
            sudo pkill -u "$SEMACS_USER" && sleep 1
        fi
    fi
}

semacs_init_server() {
    semacs_kill_server
    _semacs_setup_server_dir

    # Start daemon
    sudo -u "$SEMACS_USER" \
         HOME="$SEMACS_DOT_EMACS" \
         emacs \
         --daemon="$SEMACS_SOCKET_FILE" \
         --init-directory="$SEMACS_DOT_EMACS" &

    sudo -u "$SEMACS_USER" ls "$SEMACS_SOCKET_FILE"
}

semacs_init_or_connect() {
    local connected=0
    if ! _semacs_is_server_running; then
        semacs_init_server
        sleep 1
    fi

    _semacs_connect_server

}

semacs_eval_elisp() {
    if _semacs_is_server_running; then
        sudo -u "$SEMACS_USER" HOME="$SEMACS_DOT_EMACS" emacsclient -s "$SEMACS_SOCKET_FILE" -e "$1"
    else
        echo "Server is not running"
        exit 1
    fi
}

_semacs_is_server_running() {
    # Check process exists
    if ! pgrep -u "$SEMACS_USER" emacs >/dev/null; then
        return 1
    fi

    # Check server is accepting connections
    if ! sudo -u "$SEMACS_USER" \
         HOME="$SEMACS_DOT_EMACS" \
         emacsclient -s \
         "$SEMACS_SOCKET_FILE" \
         -e '(version)' \
         >/dev/null 2>&1; then
        return 1
    fi

    return 0
}

_semacs_setup_server_dir() {
    sudo rm -rf "$SEMACS_SOCKET_DIR"
    sudo -u "$SEMACS_USER" mkdir -p "$SEMACS_SOCKET_DIR"
    sudo chmod 700 "$SEMACS_SOCKET_DIR"
    sudo chown "$SEMACS_USER":"$SEMACS_USER" "$SEMACS_SOCKET_DIR"
    # sudo chmod 770 "$SEMACS_SOCKET_DIR"
    # sudo chmod 770 "$SEMACS_SOCKET_FILE"
    sudo chown "$SEMACS_USER":"$SEMACS_USER" "$SEMACS_SOCKET_DIR"
}

# _semacs_connect_server() {
#     sudo -u "$SEMACS_USER" HOME="$SEMACS_HOME" emacsclient -s "$SEMACS_SOCKET_FILE" -c &
# }

_semacs_connect_server() {
    sudo -u "$SEMACS_USER" HOME="$SEMACS_DOT_EMACS" emacsclient -s "$SEMACS_SOCKET_FILE" -c &
}

case "$COMMAND" in
    start)   semacs_init_or_connect & ;;
    kill)    semacs_kill_server & ;;
    init) semacs_kill_server && semacs_init_or_connect & ;;
    status)  _semacs_is_server_running ;;
    execute) semacs_eval_elisp "$2" & ;;
    help)    show_help ;;
    *)       show_help ;;
esac


# EOF
