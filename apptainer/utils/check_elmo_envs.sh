#!/bin/bash
# Time-stamp: "2024-12-23 18:35:17 (ywatanabe)"
# File: ./ELMO/src/apptainer_builders/check_elmo_environment.sh

# Source environment variables
THIS_DIR="$(dirname $0)"
source "/opt/elmo/config/env/00_all.env"


check_elmo_envs() {
    # Set ELMO_ID before checking variables
    export ELMO_ID=1
    source "/opt/elmo/config/env/00_all.env"

    echo "=== ELMO Environment Variables for $ELMO_USER==="
    compgen -v | grep '^ELMO\|^HOST' | while read var; do
        echo "$var=${!var}"
    done
}

check_user_setup() {
    echo -e "\n=== Checking User Setup ==="
    for i in $(seq 1 $ELMO_N_AGENTS); do
        local user="elmo-$(printf "%03d" $i)"
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
    for i in $(seq 1 $ELMO_N_AGENTS); do
        local user="elmo-$(printf "%03d" $i)"
        local workspace="/workspace/$user"
        echo "Checking $workspace permissions..."
        ls -ld "$workspace" 2>/dev/null || echo "Workspace for $user not found!"
        for dir in src logs config requests backups; do
            ls -ld "$workspace/$dir" 2>/dev/null || echo "$dir for $user not found!"
        done
    done
}

check_user_environment() {
    local test_user="elmo-001"
    echo -e "\n=== Checking User Environment for $test_user ==="
    su - $test_user -c "
        export ELMO_ID=1
        source /opt/elmo/config/env/00_all.env
        env | grep '^ELMO'
        echo \"Python venv: \$ELMO_PYTHON_VIRTUAL_ENV\"
        which python
    "
}


# Run all checks
check_elmo_envs
check_user_setup
check_workspace_structure
check_permissions
check_user_environment


# === ELMO Environment Variables for ===
# HOSTNAME=ywata-note-win
# HOSTTYPE=x86_64
# ELMO_BASE_UID=9999
# ELMO_GROUP=elmo
# ELMO_HOME=/opt/elmo
# ELMO_HOST_HOME=/home/ywatanabe
# ELMO_HOST_USER=ywatanabe
# ELMO_HOST_WORKSPACE=/home/ywatanabe/.emacs.d/lisp/ELMO/config/env/../../workspace
# ELMO_ID=1
# ELMO_LLM_API_KEY=
# ELMO_LLM_ENGINE=
# ELMO_N_AGENTS=2
# ELMO_ROOT=/opt/elmo
# ELMO_ROOT_APPTAINER=/opt/elmo
# ELMO_ROOT_HOST=/home/ywatanabe/.emacs.d/lisp/ELMO/config/env/../..
# bash: /opt/elmo/config/env/ENVS.sh.src: No such file or directory
# (A) root $ check_user_setup

# === Checking User Setup ===
# Checking elmo-001...
# uid=10000(elmo-001) gid=10000(elmo-001) groups=10000(elmo-001),100(users),1000(elmo)
# lrwxrwxrwx 1 elmo-001 elmo 32 Dec 23 17:25 /home/elmo-001 -> /workspace/elmos/elmo-001/home
# -rwxrwxrwx 1 elmo-001 elmo 252 Dec 23 17:25 /home/elmo-001/.bashrc
# total 12
# drwxrwsrwx 3 elmo-001 elmo 4096 Dec 23 17:26 .
# drwxrwsrwx 4 elmo-001 elmo 4096 Dec 23 17:25 ..
# drwx--S--- 2 elmo-001 elmo 4096 Dec 23 17:26 emacs-server
# Checking elmo-002...
# uid=10001(elmo-002) gid=10001(elmo-002) groups=10001(elmo-002),100(users),1000(elmo)
# lrwxrwxrwx 1 elmo-002 elmo 32 Dec 23 17:25 /home/elmo-002 -> /workspace/elmos/elmo-002/home
# -rwxrwxrwx 1 elmo-002 elmo 252 Dec 23 17:25 /home/elmo-002/.bashrc
# total 12
# drwxrwsrwx 3 elmo-002 elmo 4096 Dec 23 17:26 .
# drwxrwsrwx 4 elmo-002 elmo 4096 Dec 23 17:25 ..
# drwx--S--- 2 elmo-002 elmo 4096 Dec 23 17:26 emacs-server
# (A) root $ check_workspace_structure

# === Checking Workspace Structure ===
# /workspace
# ├── elmos
# │   ├── elmo-001
# │   │   └── home
# │   └── elmo-002
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
# Checking /workspace/elmo-001 permissions...
# Workspace for elmo-001 not found!
# src for elmo-001 not found!
# logs for elmo-001 not found!
# config for elmo-001 not found!
# requests for elmo-001 not found!
# backups for elmo-001 not found!
# Checking /workspace/elmo-002 permissions...
# Workspace for elmo-002 not found!
# src for elmo-002 not found!
# logs for elmo-002 not found!
# config for elmo-002 not found!
# requests for elmo-002 not found!
# backups for elmo-002 not found!
# (A) root $ check_user_environment

# === Checking User Environment for elmo-001 ===
# -bash: line 3: /opt/elmo/src/shell/ENVS.sh.src: No such file or directory
# ELMO_LLM_API_KEY=
# ELMO_LLM_ENGINE=
# ELMO_ROOT_APPTAINER=/opt/elmo
# ELMO_BASE_UID=9999
# ELMO_HOST_WORKSPACE=/opt/elmo/config/env/../../workspace
# ELMO_GROUP=elmo
# ELMO_ID=1
# ELMO_ROOT_HOST=/opt/elmo/config/env/../..
# ELMO_ROOT=/opt/elmo/config/env/../..
# ELMO_HOST_HOME=/home/elmo-001
# ELMO_N_AGENTS=2
# ELMO_HOST_USER=elmo-001
# Python venv: 
# /usr/bin/python
# (A) root $ 
# (A) root $ 
# EOF
