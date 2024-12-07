#!/bin/bash
# Time-stamp: "2024-12-08 03:57:31 (ywatanabe)"
# File: ./Semacs/src/shell/create_semacs_user.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script must be run as root" >&2
   exit 1
fi

# Create user if not exists
if ! id "$SEMACS_USER" &>/dev/null; then
    useradd -m -s /bin/bash "$SEMACS_USER"
fi

# Create home directory
mkdir -p "$SEMACS_HOME"
mkdir -p "$SEMACS_EMACS_HOME"

# Set ownership
chown -R "$SEMACS_USER:$SEMACS_USER" "$SEMACS_HOME"
chown -R "$SEMACS_USER:$SEMACS_USER" "$SEMACS_EMACS_HOME"

# EOF



# EOF
