#!/bin/bash
# Time-stamp: "2024-12-22 20:22:45 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/building/user-setup/environment/03_python_env.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/Ninja/config/env/00_all.env

create_ninjas_python_envs() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_python_env $ninja_id
    done
}

create_ninja_python_env() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id    
    
    # Set up Python virtual environment
    python3 -m venv $NINJA_HOME/.env >/dev/null

    # Install Python packages
    . $NINJA_HOME/.env/bin/activate >/dev/null
    pip install --upgrade pip >/dev/null
    pip install -r $THIS_DIR/requirements.txt >/dev/null

    chmod -R 770 $NINJA_HOME/.env
    chown -R $NINJA_USER:$NINJA_GROUP $NINJA_HOME/.env    
}

create_ninjas_python_envs

# EOF
