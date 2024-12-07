#!/bin/bash
# Time-stamp: "2024-12-08 07:49:04 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/install_python.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

THIS_DIR="$(dirname $0)"

apt-get install -y \
    python3 \
    python3-pip \
    python3-venv

VENV_DIR=/workspace/.env
# Set up Python virtual environment
sudo -u $SEMACS_USER bash -c "python3 -m venv $VENV_DIR"

# Install Python packages
sudo -u $SEMACS_USER bash -c ". /workspace/.env/bin/activate && pip install --upgrade pip"
sudo -u $SEMACS_USER bash -c ". /workspace/.env/bin/activate && pip install $THIS_DIR/requirements.txt"




# EOF
