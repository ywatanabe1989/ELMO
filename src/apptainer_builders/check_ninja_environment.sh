#!/bin/bash
# Time-stamp: "2024-12-11 14:04:29 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/check_ninja_environment.sh

# Source environment variables
THIS_DIR="$(dirname $0)"
source "${THIS_DIR}/ENVS.sh.src"


# check_ninja_envs() {
#     echo "=== NINJA Environment Variables ==="
#     compgen -v | grep '^NINJA\|^HOST' | while read var; do
#         echo "$var=${!var}"
#     done
# }

check_ninja_envs() {
    # Set NINJA_ID before checking variables
    export NINJA_ID=1
    source "${THIS_DIR}/ENVS.sh.src"

    echo "=== NINJA Environment Variables for $NINJA_USER==="
    compgen -v | grep '^NINJA\|^HOST' | while read var; do
        echo "$var=${!var}"
    done

    unset NINJA_ID
    source "${THIS_DIR}/ENVS.sh.src"
}

# check_basic_envs() {
#     echo "=== Checking Basic Environment Variables ==="
#     local vars=(NINJAS_GROUP NINJA_N_AGENTS)
#     for var in "${vars[@]}"; do
#         echo "$var: ${!var}"
#     done
# }

check_user_setup() {
    echo -e "\n=== Checking User Setup ==="
    for i in $(seq 1 $NINJA_N_AGENTS); do
        local user="ninja-$(printf "%03d" $i)"
        echo "Checking $user..."
        id $user 2>/dev/null || echo "User $user not found!"
        ls -ld "/home/$user" 2>/dev/null || echo "Home dir for $user not found!"
        ls -la "/home/$user/.bashrc" 2>/dev/null || echo ".bashrc for $user not found!"
        ls -la "/home/$user/.emacs.d" 2>/dev/null || echo ".emacs.d for $user not found!"
    done
}

check_workspace_structure() {
    echo -e "\n=== Checking Workspace Structure ==="
    tree /workspace
}

check_permissions() {
    echo -e "\n=== Checking Permissions ==="
    for i in $(seq 1 $NINJA_N_AGENTS); do
        local user="ninja-$(printf "%03d" $i)"
        local workspace="/workspace/$user"
        echo "Checking $workspace permissions..."
        ls -ld "$workspace" 2>/dev/null || echo "Workspace for $user not found!"
        for dir in src logs config requests backups; do
            ls -ld "$workspace/$dir" 2>/dev/null || echo "$dir for $user not found!"
        done
    done
}

# check_user_environment() {
#     echo -e "\n=== Checking User Environment ==="
#     local test_user="ninja-001"
#     echo "Testing as $test_user..."
#     su - $test_user -c '
#         echo "NINJA_USER: $NINJA_USER"
#         echo "NINJA_WORKSPACE: $NINJA_WORKSPACE"
#         echo "Python venv: "
#         ls $NINJA_PYTHON_VIRTUAL_ENV/bin/python 2>/dev/null || echo "Python venv not found!"
#     '
# }
check_user_environment() {
    local test_user="ninja-001"
    echo -e "\n=== Checking User Environment for $test_user ==="
    su - $test_user -c "
        export NINJA_ID=1
        source /opt/Ninja/src/shell/ENVS.sh.src
        env | grep '^NINJA'
        echo \"Python venv: \$NINJA_PYTHON_VIRTUAL_ENV\"
        which python
    "
}


# Run all checks
check_ninja_envs
check_user_setup
check_workspace_structure
check_permissions
check_user_environment


# EOF
