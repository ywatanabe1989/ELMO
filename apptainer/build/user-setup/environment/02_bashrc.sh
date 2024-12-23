#!/bin/bash
# Time-stamp: "2024-12-23 16:18:54 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/building/user-setup/environment/02_bashrc.sh

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/Ninja/config/env/00_all.env

setup_ninjas_bashrc() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        setup_ninja_bashrc $ninja_id
        setup_ninja_login_shell_as_bash $ninja_id
    done
}

setup_ninja_login_shell_as_bash() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # Set bash as login shell for the ninja user
    chsh -s /bin/bash $NINJA_USER

    # Verify shell change
    if ! grep -q "^$NINJA_USER.*bash$" /etc/passwd; then
        echo "Failed to set bash as login shell for $NINJA_USER" >&2
        exit 1
    fi
}

setup_ninja_bashrc() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id
    NINJA_BASHRC=$NINJA_USER_HOME/.bashrc

    cat > $NINJA_BASHRC << EOF
# ----------------------------------------
# $0
# ----------------------------------------
export NINJA_ID=$NINJA_ID
source /opt/Ninja/config/env/00_all.env
# ----------------------------------------
EOF

    if [ ! -r "$NINJA_BASHRC" ]; then
        echo "Error: $NINJA_BASHRC is not readable" >&2
        return
    fi

    if ! grep -q "export NINJA_ID=$NINJA_ID" "$NINJA_BASHRC"; then
        echo "Error: NINJA_ID not set correctly in $NINJA_BASHRC" >&2
        return
    fi

    cmd=$(cat $NINJA_BASHRC) && echo "$cmd" && eval "$cmd" && echo ""
}

setup_ninjas_bashrc

# EOF
