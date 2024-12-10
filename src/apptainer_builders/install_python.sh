#!/bin/bash
# Time-stamp: "2024-12-10 09:22:58 (ywatanabe)"
# File: ./ninja/src/apptainer_builders/install_python.sh

THIS_DIR="$(dirname $0)"

apt install -y \
    python3 \
    python3-pip \
    python3-venv


# Set up Python virtual environment
python3 -m venv $PYTHON_VIRTUAL_ENV

# Install Python packages
. $PYTHON_VIRTUAL_ENV/bin/activate && pip install --upgrade pip
. $PYTHON_VIRTUAL_ENV/bin/activate && pip install -r $THIS_DIR/requirements.txt

# Fixme: chown: invalid user: 'ninja:ninja'
# # Set permissions for NINJA_USER
# chown -R $NINJA_USER:$NINJA_GROUP $PYTHON_VIRTUAL_ENV

# EOF
