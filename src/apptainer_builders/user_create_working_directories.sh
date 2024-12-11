#!/bin/bash
# Time-stamp: "2024-12-11 12:20:27 (ywatanabe)"
# File: ./Ninja/src/shell/apptainer_builders/make_directories.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

# Source environment variables
source "$(dirname $0)"/ENVS.sh.src

make_shared_working_directories() {
    mkdir -p \
          "$NINJA_SHARED_WORKSPACE" \
          "$NINJA_SHARED_SRC" \
          "$NINJA_SHARED_LOGS" \
          "$NINJA_SHARED_CONFIG" \
          "$NINJA_SHARED_REQUESTS" \
          "$NINJA_SHARED_BACKUPS" \
          >/dev/null
    }

make_ninjas_working_directories() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        make_ninja_working_directories $ninja_id >/dev/null
    done

}

make_ninja_working_directories() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id
    mkdir -p \
          "$NINJA_WORKSPACE" \
          "$NINJA_SRC" \
          "$NINJA_LOGS" \
          "$NINJA_CONFIG" \
          "$NINJA_REQUESTS" \
          "$NINJA_BACKUPS" \
          >/dev/null
}

# Make ninjas' working directories
make_shared_working_directories >/dev/null
make_ninjas_working_directories >/dev/null


# EOF
