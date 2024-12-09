#!/bin/bash
# Time-stamp: "2024-12-09 10:29:10 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/install_python.sh

THIS_DIR="$(dirname $0)"

apt install -y \
    python3 \
    python3-pip \
    python3-venv

# Set up Python virtual environment
sudo -u $SEMACS_USER bash -c "python3 -m venv $PYTHON_VIRTUAL_ENV"

# Install Python packages
sudo -u $SEMACS_USER bash -c ". $PYTHON_VIRTUAL_ENV/bin/activate && pip install --upgrade pip"
sudo -u $SEMACS_USER bash -c ". $PYTHON_VIRTUAL_ENV/bin/activate && pip install $THIS_DIR/requirements.txt"




# EOF
