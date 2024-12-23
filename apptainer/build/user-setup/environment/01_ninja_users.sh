#!/bin/bash
# Time-stamp: "2024-12-23 16:49:20 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/Ninja/apptainer/building/user-setup/environment/01_ninja_users.sh

echo "$0..."
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ ! -n "$APPTAINER_CONTAINER" ]; then
    echo $APPTAINER_CONTAINER
    echo "This script ($0) must be run in Apptainer Container" >&2
    exit 0
fi

if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 0
fi

delete_ubuntu_user() {
    /sbin/deluser ubuntu 2>/dev/null && echo "Deleted ubuntu user"
}

create_ninja_group() {
    /sbin/groupadd -f $NINJA_GROUP >/dev/null && echo "Created ninja group: $NINJA_GROUP"
}

create_ninja_users() {
    echo "Creating $NINJA_N_AGENTS ninja users..."
    for NINJA_ID in $(seq 1 $NINJA_N_AGENTS); do
        create_ninja_user $NINJA_ID
    done
    echo "All ninja users created successfully"
}

create_ninja_user() {
    local ninja_id="$1"
    update_ninja_envs $ninja_id

    # Cleanup if exists
    /sbin/groupdel $NINJA_USER 2>/dev/null || true
    /sbin/userdel $NINJA_USER 2>/dev/null || true
    rm -rf $NINJA_USER_HOME 2>/dev/null || true
    rm -rf /home/$NINJA_USER 2>/dev/null || true
    mkdir -p "$NINJA_USER_HOME" >/dev/null
    mkdir -p "$NINJA_USER_HOME/.emacs.d" >/dev/null
    
    # Main
    /sbin/adduser \
        --uid $NINJA_UID \
        --shell /usr/bin/bash \
        --home $NINJA_USER_HOME \
        --disabled-password \
        --gecos "" \
        $NINJA_USER \
        >/dev/null

    passwd -d $NINJA_USER >/dev/null
    # chown -R "${NINJA_USER}:${NINJA_GROUP}" "$NINJA_USER_HOME" >/dev/null

    # Group
    /sbin/usermod -aG $NINJA_GROUP $NINJA_USER >/dev/null

    # Symlink from /home/ninja-<id> to $NINJA_USER_HOME (/workspace/ninjas/ninja-<id>/home)
    ln -sfn "$NINJA_USER_HOME" "/home/$NINJA_USER" >/dev/null

    echo ""
    echo -e "\nCreated user: $NINJA_USER (uid: $NINJA_UID, home: $NINJA_USER_HOME)"
    cmd="ls -al $NINJA_USER_HOME" && echo $cmd && eval $cmd
    echo ""
    cmd="ls -al /home/$NINJA_USER" && echo $cmd && eval $cmd
    echo ""    
}

# Main
delete_ubuntu_user
create_ninja_group
create_ninja_users

# EOF
