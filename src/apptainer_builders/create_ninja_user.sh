#!/bin/bash
# Time-stamp: "2024-12-09 23:11:49 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/create_ninja_user.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

export PATH="/sbin:$PATH"

create_ninja_group() {
    groupadd -f $NINJA_AGENTS_GROUP
}

create_user_user() {
    groupadd -g $HOST_UID $HOST_USER

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
    groupadd -g $NINJA_UID $NINJA_USER

    # Remove existing user if exists (ignore errors)
    userdel $NINJA_USER 2>/dev/null || true
    rm -rf $NINJA_HOME 2>/dev/null || true

    # User creation
    adduser \
        --disabled-password \
        --gecos "" \
        --uid $NINJA_UID \
        --shell /bin/bash \
        --home $NINJA_HOME \
        $NINJA_USER

    # Add user to group
    usermod -aG $NINJA_USER $NINJA_USER
    usermod -aG $NINJA_AGENTS_GROUP $NINJA_USER

    # Create home directory
    mkdir -p "$NINJA_HOME"
    mkdir -p "$NINJA_DOT_EMACS"

    # Set ownership
    chown -R "$NINJA_USER:$NINJA_GROUP" "$NINJA_HOME"
    chown -R "$NINJA_USER:$NINJA_GROUP" "$NINJA_DOT_EMACS"
}


# Create users in correct order
create_ninja_group
create_user_user
create_ninja_user

# EOF
