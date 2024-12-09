#!/bin/bash
# Time-stamp: "2024-12-08 07:49:01 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/start_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.source

su $SEMACS_USER

# Kill existing emacs servers
emacsclient -e '(kill-emacs)' || true

# Run Emacs as daemon
emacs --daemon=$SEMACS_SERVER_SOCKET

for i in {1..30}; do
    if emacsclient -e '(+ 1 2)' >/dev/null 2>&1; then
        emacsclient --socket-name $SEMACS_SERVER_SOCKET -c -n
        break
    fi
    sleep 1
done


# EOF
