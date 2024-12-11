#!/bin/bash
# Time-stamp: "2024-12-11 13:11:03 (ywatanabe)"
# File: ./ninja/src/apptainer_builders/install_python.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

THIS_DIR="$(dirname $0)"
source "$(dirname $0)"/ENVS.sh.src

create_ninjas_python_envs() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_python_env $ninja_id
    done
}

create_ninja_python_env() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id    
    
    # Set up Python virtual environment
    python3 -m venv $NINJA_PYTHON_VIRTUAL_ENV >/dev/null

    # Install Python packages
    . $NINJA_PYTHON_VIRTUAL_ENV/bin/activate && \
        pip install --upgrade pip && \
        pip install -r $THIS_DIR/requirements.txt &&
        >/dev/null
}

create_ninjas_python_envs
# export -f create_ninja_python_env
# export THIS_DIR NINJA_PYTHON_VIRTUAL_ENV

# # Parallel execution
# seq 1 $NINJA_N_AGENTS | parallel -j$(nproc) create_ninja_python_env {}


# EOF
