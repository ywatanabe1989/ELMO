#!/bin/bash
# Time-stamp: "2024-12-23 16:49:20 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/building/user-setup/environment/01_elmo_users.sh

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

create_elmo_group() {
    /sbin/groupadd -f $ELMO_GROUP >/dev/null && echo "Created elmo group: $ELMO_GROUP"
}

create_elmo_users() {
    echo "Creating $ELMO_N_AGENTS elmo users..."
    for ELMO_ID in $(seq 1 $ELMO_N_AGENTS); do
        create_elmo_user $ELMO_ID
    done
    echo "All elmo users created successfully"
}

create_elmo_user() {
    local elmo_id="$1"
    update_elmo_envs $elmo_id

    # Cleanup if exists
    /sbin/groupdel $ELMO_USER 2>/dev/null || true
    /sbin/userdel $ELMO_USER 2>/dev/null || true
    rm -rf $ELMO_USER_HOME 2>/dev/null || true
    rm -rf /home/$ELMO_USER 2>/dev/null || true
    mkdir -p "$ELMO_USER_HOME" >/dev/null
    mkdir -p "$ELMO_USER_HOME/.emacs.d" >/dev/null
    
    # Main
    /sbin/adduser \
        --uid $ELMO_UID \
        --shell /usr/bin/bash \
        --home $ELMO_USER_HOME \
        --disabled-password \
        --gecos "" \
        $ELMO_USER \
        >/dev/null

    passwd -d $ELMO_USER >/dev/null
    # chown -R "${ELMO_USER}:${ELMO_GROUP}" "$ELMO_USER_HOME" >/dev/null

    # Group
    /sbin/usermod -aG $ELMO_GROUP $ELMO_USER >/dev/null

    # Symlink from /home/elmo-<id> to $ELMO_USER_HOME (/workspace/elmos/elmo-<id>/home)
    ln -sfn "$ELMO_USER_HOME" "/home/$ELMO_USER" >/dev/null

    echo ""
    echo -e "\nCreated user: $ELMO_USER (uid: $ELMO_UID, home: $ELMO_USER_HOME)"
    cmd="ls -al $ELMO_USER_HOME" && echo $cmd && eval $cmd
    echo ""
    cmd="ls -al /home/$ELMO_USER" && echo $cmd && eval $cmd
    echo ""    
}

# Main
delete_ubuntu_user
create_elmo_group
create_elmo_users

# EOF
