#!/bin/bash
# Time-stamp: "2024-12-11 12:33:38 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

create_ninjas_emacsd() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_emacsd $ninja_id
    done
}

create_ninja_emacsd() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id    

    # Initialization
    rm -rf $NINJA_HOME_EMACSD

    # Softlink; (is copying better)
    ln -sf /opt/Ninja/src/apptainer_builders/shared_emacsd $NINJA_HOME_EMACSD
    ln -sf /opt/Ninja/src/apptainer_builders/start_emacs.sh $NINJA_HOME/
    }

create_ninjas_emacsd

# EOF
