#!/bin/bash
# Time-stamp: "2024-12-23 16:22:06 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/building/user-setup/environment/03_python_env.sh

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/ELMO/config/env/00_all.env

create_elmos_python_envs() {
    for elmo_id in $(seq 1 $ELMO_N_AGENTS); do
        create_elmo_python_env $elmo_id
    done
}

create_elmo_python_env() {
    local elmo_id="$1"
    update_elmo_envs $elmo_id    
    
    # Set up Python virtual environment
    python3 -m venv $ELMO_USER_HOME/.env >/dev/null

    # Install Python packages
    . $ELMO_USER_HOME/.env/bin/activate >/dev/null
    pip install --upgrade pip >/dev/null
    pip install -r /opt/ELMO/config/env/requirements.txt >/dev/null
}

create_elmos_python_envs

# EOF
