#!/bin/bash
# Time-stamp: "2024-12-05 19:47:08 (ywatanabe)"
# File: ./self-evolving-agent/src/.sea_server-versions/sea_server_v001.sh

SEA_USER="${SEA_USER:-sea}"
SEA_HOME=/home/"${SEA_USER:-sea}"
SEA_UID=$(id -u "$SEA_USER")
SEA_EMACS_SERVER_DIR=/tmp/emacs"$SEA_UID"
SEA_SOCKET_FILE="$SEA_EMACS_SERVER_DIR/server"

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
    -u USER     SEA user (default: $SEA_USER)
    -s SOCKET   Socket name (default: $SEA_SOCKET_NAME)
    -h          Show this help message
EOF
    exit 0
}

# Argument parser
while getopts "u:s:h" opt; do
    case $opt in
        u) SEA_USER="$OPTARG" ;;
        s) SEA_SOCKET_NAME="$OPTARG" ;;
        h) show_help ;;
        ?) show_help ;;
    esac
done

shift $((OPTIND-1))
COMMAND="${1:-start}"

SEA_UID=$(id -u "$SEA_USER")
SEA_EMACS_SERVER_DIR=/tmp/emacs"$SEA_UID"

sea_kill_server() {
    if _sea_is_server_running; then
        sudo -u "$SEA_USER" emacsclient -s "$SEA_SOCKET_FILE" -e '(kill-emacs)' && sleep 1
        if _sea_is_server_running; then
            sudo pkill -u "$SEA_USER" && sleep 1
        fi
    fi
}

sea_init_server() {
    sea_kill_server
    _sea_setup_server_dir
    # sudo -u "$SEA_USER" emacs --daemon
    sudo -u "$SEA_USER" HOME="$SEA_HOME" emacs --daemon
}

sea_init_or_connect() {
    if ! _sea_is_server_running; then
        sea_init_server
        while ! _sea_is_server_running; do
            sleep 1
            echo "waiting for server..."
        done
        sleep 1
    fi
    _sea_connect_server
}


sea_execute() {
    if _sea_is_server_running; then
        sudo -u "$SEA_USER" HOME="$SEA_HOME" emacsclient -s "$SEA_SOCKET_FILE" -e "$1"
    else
        echo "Server is not running"
        exit 1
    fi
}

_sea_is_server_running() {
    pgrep -u "$SEA_USER" emacs >/dev/null
    return $?
}

_sea_setup_server_dir() {
    sudo rm -rf "$SEA_EMACS_SERVER_DIR"
    sudo -u "$SEA_USER" mkdir -p "$SEA_EMACS_SERVER_DIR"
    sudo chmod 700 "$SEA_EMACS_SERVER_DIR"
    sudo chown "$SEA_USER":"$SEA_USER" "$SEA_EMACS_SERVER_DIR"
}

_sea_connect_server() {
    sudo -u "$SEA_USER" HOME="$SEA_HOME" emacsclient -s "$SEA_SOCKET_FILE" -c
}



case "$COMMAND" in
    start)   sea_init_or_connect ;;
    kill)    sea_kill_server ;;
    init) sea_kill_server && sea_init_or_connect ;;
    status)  _sea_is_server_running && echo "Server is running" || echo "Server is not running" ;;
    execute) sea_execute "$2" ;;
    help)    show_help ;;
    *)       show_help ;;
esac

# case "$COMMAND" in
#     start)   sea_init_or_connect ;;
#     kill)    sea_kill_server ;;
#     init) sea_kill_server && sea_init_or_connect ;;
#     status)  _sea_is_server_running && echo "Server is running" || echo "Server is not running" ;;
#     execute)
#         if _sea_is_server_running; then
#             sudo -u "$SEA_USER" emacsclient -s "$SEA_SOCKET_FILE" -e "$2"
#         else
#             echo "Server is not running"
#             exit 1
#         fi
#         ;;
#     help)    show_help ;;
#     *)       show_help ;;
# esac


# EOF
