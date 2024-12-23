#!/bin/bash
# Time-stamp: "2024-12-23 18:29:20 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/building/user-setup/permissions/00_all.sh

set -e

if [ ! -n "$APPTAINER_CONTAINER" ]; then
    echo $APPTAINER_CONTAINER
    echo "This script ($0) must be run in Apptainer Container" >&2
    exit 0
fi

# Setup logging
LOG_FILE="/var/log/elmo-permissions.log"
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Validation
validate_environment() {
    if ! getent group "$ELMO_GROUP" >/dev/null; then
        log_message "ERROR: Group $ELMO_GROUP does not exist"
        exit 1
    fi
}

# Backup function
# backup_directory() {
#     local dir=$1
#     if [ -d "$dir" ]; then
#         backup_path="${dir}_backup_$(date +%Y%m%d_%H%M%S)"
#         cp -r "$dir" "$backup_path"
#         log_message "Backup created: $backup_path"
#     fi
# }

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source /opt/elmo/config/env/00_all.env

validate_environment

correct_root_777() {
    # World readable/writable directories
    DIRS="/workspace"
    for dir in $DIRS; do
        log_message "Setting 777 permissions for $dir"
        # backup_directory "$dir"
        mkdir -p "$dir" 2>/dev/null || true
        chown -R root:"$ELMO_GROUP" "$dir" || true
        chmod -R 777 "$dir" || true
    done
}

correct_root_775() {
    # System directories
    DIRS="/usr/bin /bin /opt/elmo"
    for dir in $DIRS; do
        log_message "Setting 775 permissions for $dir"
        backup_directory "$dir"
        mkdir -p "$dir" 2>/dev/null || true
        chown -R root:"$ELMO_GROUP" "$dir" || true
        chmod -R 775 "$dir" || true
    done
}

correct_elmo_777() {
    # ELMO HOME
    for elmo_id in $(seq 1 $ELMO_N_AGENTS); do
        update_elmo_envs "$elmo_id" || true
        for dir in "/workspace/elmos/$ELMO_USER" "/home/$ELMO_USER"; do
            log_message "Setting 777 permissions for $dir"
            backup_directory "$dir"
            echo $ELMO_USER_HOME
            mkdir -p "$dir" 2>/dev/null || true
            chown -R "$ELMO_USER":"$ELMO_GROUP" "$dir" || true
            chmod -R 777 "$dir" || true
        done
    done
}

correct_elmo_700() {
    # ELMO Emacs server socket
    for elmo_id in $(seq 1 $ELMO_N_AGENTS); do
        update_elmo_envs "$elmo_id" || true
        dir="$ELMO_USER_HOME/.emacs.d/emacs-server"
        log_message "Setting 700 permissions for $dir"
        backup_directory "$dir"
        mkdir -p "$dir" 2>/dev/null || true
        chown -R "$ELMO_USER":"$ELMO_GROUP" "$dir" || true
        chmod -R 700 "$dir" || true
    done
}

log_message "Starting permission corrections"
correct_root_777
correct_root_775
correct_elmo_777
correct_elmo_700
log_message "Permission corrections completed"


TREE_OPTIONS="-al --prune -F -p -u -g --dirsfirst --gitignore -L 3"
tree /workspace $TREE_OPTIONS || true
tree /home/elmo-001 $TREE_OPTIONS || true
tree /workspace/elmos/elmo-001/home $TREE_OPTIONS || true

# EOF
