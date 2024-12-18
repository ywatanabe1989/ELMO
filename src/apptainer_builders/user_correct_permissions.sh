#!/bin/bash
# Time-stamp: "2024-12-18 21:25:25 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_correct_permissions.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
    echo "This script ($0) must be run as root" >&2
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$THIS_DIR"/ENVS.sh.src
source "$THIS_DIR"/user_correct_permissions_emacsd.sh.src

correct_permissions_home() {
    for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
        update_ninja_envs $ninja_id

        chmod -R 750 $NINJA_HOME >/dev/null 2>&1
        chown -R $NINJA_USER:$NINJAS_GROUP $NINJA_HOME >/dev/null 2>&1        
    done
}

correct_permissions_global(){
    chown -R root:$NINJAS_GROUP /home >/dev/null 2>&1
    chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null 2>&1
    chmod -R 770 /opt/Ninja/src/apptainer_builders/shared_emacsd >/dev/null 2>&1
}

correct_permissions_shared() {
    chmod -R 750 -R /opt/Ninja >/dev/null 2>&1
    chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null 2>&1
}

correct_permissions_home
correct_permissions_shared
correct_permissions_global
# correct_permissions_emacsd
emacsd_correct_permissions

# EOF

# #!/bin/bash
# # Time-stamp: "2024-12-18 18:36:17 (ywatanabe)"
# # File: ./Ninja/src/apptainer_builders/user_correct_permissions.sh

# # Check if running as root
# if [ "$(id -u)" != "0" ]; then
#    echo "This script ($0) must be run as root" >&2
#    exit 1
# fi

# source "$(dirname $0)"/ENVS.sh.src
# source "$(dirname $0)"/user_correct_permissions_emacsd.sh.src

# correct_permissions_home() {
#     for ninja_id in $(seq 1 $NINJA_N_AGENTS); do
#         _correct_permissions_home $ninja_id
#     done
# }

# _correct_permissions_home() {
#     local ninja_id="$1"
#     update_ninja_envs $ninja_id

#     # Ninja HOME: Readable by other ninjas
#     chmod -R 750 $NINJA_HOME >/dev/null 2>&1
#     chown -R $NINJA_USER:$NINJAS_GROUP $NINJA_HOME >/dev/null 2>&1
# }

# correct_permissions_global(){
#     chown -R root:$NINJAS_GROUP /home
#     chown -R root:$NINJAS_GROUP /opt/Ninja
#     chmod -R 770 /opt/Ninja/src/apptainer_builders/shared_emacsd
# }

# correct_permissions_shared() {
#     # Source
#     chmod -R 750 -R /opt/Ninja
#     chown -R root:$NINJAS_GROUP /opt/Ninja >/dev/null 2>&1
# }

# correct_permissions_all
# correct_permissions_shared
# correct_permissions_global
# correct_permissions_emacsd

# # EOF
