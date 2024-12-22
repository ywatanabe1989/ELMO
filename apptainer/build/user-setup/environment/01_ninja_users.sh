#!/bin/bash
# Time-stamp: "2024-12-22 20:22:44 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/building/user-setup/environment/01_ninja_users.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source /opt/Ninja/config/env/00_all.env

create_ninja_group() {
    groupadd -f $NINJA_GROUP >/dev/null
}

check_users() {
    awk -F: '{print $1, $3}' /etc/passwd
}

create_ninja_users() {
    for NINJA_ID in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_user $NINJA_ID >/dev/null
    done
}

create_ninja_user() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id >/dev/null

    groupdel $NINJA_USER 2>/dev/null || true
    userdel $NINJA_USER 2>/dev/null || true
    rm -rf $NINJA_HOME 2>/dev/null || true

    adduser \
        --disabled-password \
        --gecos "" \
        --uid $NINJA_UID \
        --shell /bin/bash \
        --home $NINJA_HOME \
        $NINJA_USER \
        >/dev/null

    # Set empty password
    passwd -d $NINJA_USER >/dev/null

    mkdir -p "$NINJA_HOME" >/dev/null
    mkdir -p "$NINJA_HOME/.emacs.d" >/dev/null
    chown -R "${NINJA_USER}:${NINJA_USER}" "$NINJA_HOME" >/dev/null

    # Group
    usermod -aG $NINJA_GROUP $NINJA_USER >/dev/null
}

delete_ubuntu_user() {
    /sbin/deluser ubuntu >/dev/null
}

# Main
delete_ubuntu_user
create_ninja_group
create_ninja_users

# EOF
