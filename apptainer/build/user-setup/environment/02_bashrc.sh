#!/bin/bash
# Time-stamp: "2024-12-23 16:18:54 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/building/user-setup/environment/02_bashrc.sh

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

source /opt/elmo/config/env/00_all.env

setup_elmos_bashrc() {
    for elmo_id in $(seq 1 $ELMO_N_AGENTS); do
        setup_elmo_bashrc $elmo_id
        setup_elmo_login_shell_as_bash $elmo_id
    done
}

setup_elmo_login_shell_as_bash() {
    local elmo_id="$1"
    update_elmo_envs $elmo_id

    # Set bash as login shell for the elmo user
    chsh -s /bin/bash $ELMO_USER

    # Verify shell change
    if ! grep -q "^$ELMO_USER.*bash$" /etc/passwd; then
        echo "Failed to set bash as login shell for $ELMO_USER" >&2
        exit 1
    fi
}

setup_elmo_bashrc() {
    local elmo_id="$1"
    update_elmo_envs $elmo_id
    ELMO_BASHRC=$ELMO_USER_HOME/.bashrc

    cat > $ELMO_BASHRC << EOF
# ----------------------------------------
# $0
# ----------------------------------------
export ELMO_ID=$ELMO_ID
source /opt/elmo/config/env/00_all.env
# ----------------------------------------
EOF

    if [ ! -r "$ELMO_BASHRC" ]; then
        echo "Error: $ELMO_BASHRC is not readable" >&2
        return
    fi

    if ! grep -q "export ELMO_ID=$ELMO_ID" "$ELMO_BASHRC"; then
        echo "Error: ELMO_ID not set correctly in $ELMO_BASHRC" >&2
        return
    fi

    cmd=$(cat $ELMO_BASHRC) && echo "$cmd" && eval "$cmd" && echo ""
}

setup_elmos_bashrc

# EOF
