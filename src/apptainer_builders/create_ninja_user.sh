#!/bin/bash
# Time-stamp: "2024-12-10 17:20:50 (ywatanabe)"
# File: ./ninja/src/apptainer_builders/create_ninja_user.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

export PATH="/sbin:$PATH"

create_ninja_group() {
    groupadd -f $NINJA_AGENTS_GROUP
}

check_users() {
    awk -F: '{print $1, $3}' /etc/passwd

    # Apptainer> check_users
    # root 0
    # daemon 1
    # bin 2
    # sys 3
    # sync 4
    # games 5
    # man 6
    # lp 7
    # mail 8
    # news 9
    # uucp 10
    # proxy 13
    # www-data 33
    # backup 34
    # list 38
    # irc 39
    # _apt 42
    # nobody 65534
    # ubuntu 1000
    # systemd-network 998
    # systemd-timesync 997
    # messagebus 100
    # systemd-resolve 996
    # postfix 101
    # ninja 9999

}

create_host_user() {
    adduser \
        --disabled-password \
        --gecos "" \
        --uid $HOST_UID \
        --shell /bin/bash \
        --home $HOST_HOME \
        $HOST_USER

    # Add user to group
    usermod -aG $HOST_USER $HOST_USER
    usermod -aG $NINJA_AGENTS_GROUP $HOST_USER

    # Sudo configuration
    echo "$HOST_USER ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/$HOST_USER
    chmod 0440 /etc/sudoers.d/$HOST_USER
}


create_ninja_user() {
    echo "Starting ninja user creation..."

    # Remove existing user if exists (ignore errors)
    echo "Removing existing user and group..."
    groupdel $NINJA_USER 2>/dev/null || true
    userdel $NINJA_USER 2>/dev/null || true
    rm -rf $NINJA_HOME 2>/dev/null || true

    # User creation
    echo "Creating new user..."
    adduser \
        --disabled-password \
        --gecos "" \
        --uid $NINJA_UID \
        --shell /bin/bash \
        --home $NINJA_HOME \
        $NINJA_USER

    # Add user to group
    echo "Adding user to groups..."
    usermod -aG $NINJA_USER $NINJA_USER
    usermod -aG $NINJA_AGENTS_GROUP $NINJA_USER

    # Create home directory
    echo "Creating directories..."
    mkdir -p "$NINJA_HOME"
    mkdir -p "$NINJA_DOT_EMACS"

    # Set ownership
    echo "Setting ownership..."
    chown -R "$NINJA_USER:$NINJA_GROUP" "$NINJA_HOME"
    chown -R "$NINJA_USER:$NINJA_GROUP" "$NINJA_DOT_EMACS"

    echo "Ninja user creation completed."
}

create_ninja_users() {
    for i in $(seq -w 1 $NINJA_N_AGENTS); do
        local NINJA_USER="ninja-$(printf "%03d" $i)"
        local NINJA_HOME="/home/${NINJA_USER}"
        local NINJA_UID=$((9000 + ${i}))

        groupdel $NINJA_USER 2>/dev/null || true
        userdel $NINJA_USER 2>/dev/null || true
        rm -rf $NINJA_HOME 2>/dev/null || true

        adduser \
            --disabled-password \
            --gecos "" \
            --uid $NINJA_UID \
            --shell /bin/bash \
            --home $NINJA_HOME \
            $NINJA_USER

        usermod -aG $NINJA_USER $NINJA_USER
        usermod -aG $NINJA_AGENTS_GROUP $NINJA_USER

        mkdir -p "$NINJA_HOME"
        mkdir -p "$NINJA_HOME/.emacs.d"

        chown -R "${NINJA_USER}:${NINJA_USER}" "$NINJA_HOME"
    done
}


# Create users in correct order
check_users
create_ninja_group
create_host_user
create_ninja_user

# EOF
