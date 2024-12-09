#!/bin/bash
# Time-stamp: "2024-12-10 08:46:20 (ywatanabe)"
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

# Set permissions for NINJA_USER
chown -R $NINJA_USER:$NINJA_USER $PYTHON_VIRTUAL_ENV

# # Set up Python virtual environment
# sudo -u $NINJA_USER bash -c "python3 -m venv $PYTHON_VIRTUAL_ENV"

# # Install Python packages
# sudo -u $NINJA_USER bash -c ". $PYTHON_VIRTUAL_ENV/bin/activate && pip install --upgrade pip"
# sudo -u $NINJA_USER bash -c ". $PYTHON_VIRTUAL_ENV/bin/activate && pip install $THIS_DIR/requirements.txt"




# EOF
