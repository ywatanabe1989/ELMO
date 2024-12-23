#!/bin/bash
# Time-stamp: "2024-12-23 18:35:17 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/check_ninja_environment.sh

# Source environment variables
THIS_DIR="$(dirname $0)"
source "/opt/Ninja/config/env/00_all.env"


check_ninja_envs() {
    # Set NINJA_ID before checking variables
    export NINJA_ID=1
    source "/opt/Ninja/config/env/00_all.env"

    echo "=== NINJA Environment Variables for $NINJA_USER==="
    compgen -v | grep '^NINJA\|^HOST' | while read var; do
        echo "$var=${!var}"
    done
}

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

check_user_environment() {
    local test_user="ninja-001"
    echo -e "\n=== Checking User Environment for $test_user ==="
    su - $test_user -c "
        export NINJA_ID=1
        source /opt/Ninja/config/env/00_all.env
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


# === NINJA Environment Variables for ===
# HOSTNAME=ywata-note-win
# HOSTTYPE=x86_64
# NINJA_BASE_UID=9999
# NINJA_GROUP=ninja
# NINJA_HOME=/opt/Ninja
# NINJA_HOST_HOME=/home/ywatanabe
# NINJA_HOST_USER=ywatanabe
# NINJA_HOST_WORKSPACE=/home/ywatanabe/.emacs.d/lisp/Ninja/config/env/../../workspace
# NINJA_ID=1
# NINJA_LLM_API_KEY=
# NINJA_LLM_ENGINE=
# NINJA_N_AGENTS=2
# NINJA_ROOT=/opt/Ninja
# NINJA_ROOT_APPTAINER=/opt/Ninja
# NINJA_ROOT_HOST=/home/ywatanabe/.emacs.d/lisp/Ninja/config/env/../..
# bash: /opt/Ninja/config/env/ENVS.sh.src: No such file or directory
# (A) root $ check_user_setup

# === Checking User Setup ===
# Checking ninja-001...
# uid=10000(ninja-001) gid=10000(ninja-001) groups=10000(ninja-001),100(users),1000(ninja)
# lrwxrwxrwx 1 ninja-001 ninja 32 Dec 23 17:25 /home/ninja-001 -> /workspace/ninjas/ninja-001/home
# -rwxrwxrwx 1 ninja-001 ninja 252 Dec 23 17:25 /home/ninja-001/.bashrc
# total 12
# drwxrwsrwx 3 ninja-001 ninja 4096 Dec 23 17:26 .
# drwxrwsrwx 4 ninja-001 ninja 4096 Dec 23 17:25 ..
# drwx--S--- 2 ninja-001 ninja 4096 Dec 23 17:26 emacs-server
# Checking ninja-002...
# uid=10001(ninja-002) gid=10001(ninja-002) groups=10001(ninja-002),100(users),1000(ninja)
# lrwxrwxrwx 1 ninja-002 ninja 32 Dec 23 17:25 /home/ninja-002 -> /workspace/ninjas/ninja-002/home
# -rwxrwxrwx 1 ninja-002 ninja 252 Dec 23 17:25 /home/ninja-002/.bashrc
# total 12
# drwxrwsrwx 3 ninja-002 ninja 4096 Dec 23 17:26 .
# drwxrwsrwx 4 ninja-002 ninja 4096 Dec 23 17:25 ..
# drwx--S--- 2 ninja-002 ninja 4096 Dec 23 17:26 emacs-server
# (A) root $ check_workspace_structure

# === Checking Workspace Structure ===
# /workspace
# ├── ninjas
# │   ├── ninja-001
# │   │   └── home
# │   └── ninja-002
# │       └── home
# ├── projects
# │   └── 000-PROJECTNAME
# │       ├── data
# │       ├── docs
# │       │   ├── project-dynamic.json
# │       │   └── project-static.json
# │       ├── README.md
# │       ├── requirements.txt
# │       ├── results
# │       └── scripts
# └── resources
# ├── private
# └── shared
# ├── agents
# ├── prompts
# │   └── 001-context-to-elisp-code.json
# ├── scripts
# │   └── json2md.sh
# ├── templates
# │   ├── agent-000.json
# │   ├── context-000.json
# │   ├── project-dynamic-000.json
# │   ├── project-static-000.json
# │   ├── prompt-000.json
# │   └── tool-000.json
# └── tools
# ├── 001-elisp.json
# ├── 002-python.json
# ├── 003-git.json
# ├── 004-github.json
# └── 005-tree.json

# 20 directories, 17 files
# (A) root $ check_permissions

# === Checking Permissions ===
# Checking /workspace/ninja-001 permissions...
# Workspace for ninja-001 not found!
# src for ninja-001 not found!
# logs for ninja-001 not found!
# config for ninja-001 not found!
# requests for ninja-001 not found!
# backups for ninja-001 not found!
# Checking /workspace/ninja-002 permissions...
# Workspace for ninja-002 not found!
# src for ninja-002 not found!
# logs for ninja-002 not found!
# config for ninja-002 not found!
# requests for ninja-002 not found!
# backups for ninja-002 not found!
# (A) root $ check_user_environment

# === Checking User Environment for ninja-001 ===
# -bash: line 3: /opt/Ninja/src/shell/ENVS.sh.src: No such file or directory
# NINJA_LLM_API_KEY=
# NINJA_LLM_ENGINE=
# NINJA_ROOT_APPTAINER=/opt/Ninja
# NINJA_BASE_UID=9999
# NINJA_HOST_WORKSPACE=/opt/Ninja/config/env/../../workspace
# NINJA_GROUP=ninja
# NINJA_ID=1
# NINJA_ROOT_HOST=/opt/Ninja/config/env/../..
# NINJA_ROOT=/opt/Ninja/config/env/../..
# NINJA_HOST_HOME=/home/ninja-001
# NINJA_N_AGENTS=2
# NINJA_HOST_USER=ninja-001
# Python venv: 
# /usr/bin/python
# (A) root $ 
# (A) root $ 
# EOF
