#!/bin/bash
# Time-stamp: "2024-12-23 12:40:41 (ywatanabe)"
# File: ./ELMO/src/apptainer_builders/install_python.sh

echo "$0..."

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source /opt/elmo/config/env/00_all.env

install_python() {
    echo "Installing Python and dependencies..."
    apt install -y \
        python3 \
        python3-pip \
        python3-venv \
        >/dev/null

    echo "Creating Python symlinks..."
    ln -s /bin/python3 /bin/python 2>/dev/null
    ln -s /usr/bin/python3 /usr/bin/python 2>/dev/null

    # Check installation
    python_path=$(which python)
    python_version=$(python3 --version)
    echo "Python path: ${python_path}"
    echo "Python version: ${python_version}"
    echo "Python installation completed successfully!"
}

install_python

# EOF
