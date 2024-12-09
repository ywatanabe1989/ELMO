#!/bin/bash
# Time-stamp: "2024-12-08 07:49:02 (ywatanabe)"
# File: ./Semacs/src/shell/apptainer_builders/create_semacs_user.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

# Create user if not exists
if ! id "$SEMACS_USER" &>/dev/null; then
    useradd -m -s /bin/bash "$SEMACS_USER"
fi

# Create home directory
mkdir -p "$SEMACS_HOME"
mkdir -p "$SEMACS_DOT_EMACS"

# Set ownership
chown -R "$SEMACS_USER:$SEMACS_USER" "$SEMACS_HOME"
chown -R "$SEMACS_USER:$SEMACS_USER" "$SEMACS_DOT_EMACS"

# ########################################
# # sea user
# ########################################
# sudo userdel $SEMACS_USER
# sudo rm -rf $SEMACS_HOME
# sudo adduser --disabled-password --gecos "" --uid $SEMACS_UID --shell /bin/bash --home $SEMACS_HOME $SEMACS_USER
# sudo usermod -aG sudo $SEMACS_USER
# echo "$SEMACS_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/sea
# chmod 0440 /etc/sudoers.d/sea


# EOF
