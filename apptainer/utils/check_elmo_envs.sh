#!/bin/bash
# Time-stamp: "2024-12-24 08:36:15 (ywatanabe)"
# File: ./LLEMACS/src/apptainer_builders/check_llemacs_environment.sh

# Source environment variables
THIS_DIR="$(dirname $0)"
source "/opt/llemacs/config/env/00_all.env"


check_llemacs_envs() {
    # Set LLEMACS_ID before checking variables
    export LLEMACS_ID=1
    source "/opt/llemacs/config/env/00_all.env"

    echo "=== LLEMACS Environment Variables for $LLEMACS_USER==="
    compgen -v | grep '^LLEMACS\|^HOST' | while read var; do
        echo "$var=${!var}"
    done
}

check_user_setup() {
    echo -e "\n=== Checking User Setup ==="
    for i in $(seq 1 $LLEMACS_N_AGENTS); do
        local user="llemacs-$(printf "%03d" $i)"
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
    for i in $(seq 1 $LLEMACS_N_AGENTS); do
        local user="llemacs-$(printf "%03d" $i)"
        local workspace="/workspace/$user"
        echo "Checking $workspace permissions..."
        ls -ld "$workspace" 2>/dev/null || echo "Workspace for $user not found!"
        for dir in src logs config requests backups; do
            ls -ld "$workspace/$dir" 2>/dev/null || echo "$dir for $user not found!"
        done
    done
}

check_user_environment() {
    local test_user="llemacs-001"
    echo -e "\n=== Checking User Environment for $test_user ==="
    su - $test_user -c "
        export LLEMACS_ID=1
        source /opt/llemacs/config/env/00_all.env
        env | grep '^LLEMACS'
        echo \"Python venv: \$LLEMACS_PYTHON_VIRTUAL_ENV\"
        which python
    "
}


# Run all checks
check_llemacs_envs
check_user_setup
check_workspace_structure
check_permissions
check_user_environment


# === LLEMACS Environment Variables for ===
# HOSTNAME=ywata-note-win
# HOSTTYPE=x86_64
# LLEMACS_BASE_UID=9999
# LLEMACS_GROUP=llemacs
# LLEMACS_HOME=/opt/llemacs
# LLEMACS_HOST_HOME=/home/ywatanabe
# LLEMACS_HOST_USER=ywatanabe
# LLEMACS_HOST_WORKSPACE=/home/ywatanabe/.emacs.d/lisp/LLEMACS/config/env/../../workspace
# LLEMACS_ID=1
# LLEMACS_LLM_API_KEY=
# LLEMACS_LLM_ENGINE=
# LLEMACS_N_AGENTS=2
# LLEMACS_ROOT=/opt/llemacs
# LLEMACS_ROOT_APPTAINER=/opt/llemacs
# LLEMACS_ROOT_HOST=/home/ywatanabe/.emacs.d/lisp/LLEMACS/config/env/../..
# bash: /opt/llemacs/config/env/ENVS.sh.src: No such file or directory
# (A) root $ check_user_setup

# === Checking User Setup ===
# Checking llemacs-001...
# uid=10000(llemacs-001) gid=10000(llemacs-001) groups=10000(llemacs-001),100(users),1000(llemacs)
# lrwxrwxrwx 1 llemacs-001 llemacs 32 Dec 23 17:25 /home/llemacs-001 -> /workspace/llemacss/llemacs-001/home
# -rwxrwxrwx 1 llemacs-001 llemacs 252 Dec 23 17:25 /home/llemacs-001/.bashrc
# total 12
# drwxrwsrwx 3 llemacs-001 llemacs 4096 Dec 23 17:26 .
# drwxrwsrwx 4 llemacs-001 llemacs 4096 Dec 23 17:25 ..
# drwx--S--- 2 llemacs-001 llemacs 4096 Dec 23 17:26 emacs-server
# Checking llemacs-002...
# uid=10001(llemacs-002) gid=10001(llemacs-002) groups=10001(llemacs-002),100(users),1000(llemacs)
# lrwxrwxrwx 1 llemacs-002 llemacs 32 Dec 23 17:25 /home/llemacs-002 -> /workspace/llemacss/llemacs-002/home
# -rwxrwxrwx 1 llemacs-002 llemacs 252 Dec 23 17:25 /home/llemacs-002/.bashrc
# total 12
# drwxrwsrwx 3 llemacs-002 llemacs 4096 Dec 23 17:26 .
# drwxrwsrwx 4 llemacs-002 llemacs 4096 Dec 23 17:25 ..
# drwx--S--- 2 llemacs-002 llemacs 4096 Dec 23 17:26 emacs-server
# (A) root $ check_workspace_structure

# === Checking Workspace Structure ===
# /workspace
# ├── llemacss
# │   ├── llemacs-001
# │   │   └── home
# │   └── llemacs-002
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
# Checking /workspace/llemacs-001 permissions...
# Workspace for llemacs-001 not found!
# src for llemacs-001 not found!
# logs for llemacs-001 not found!
# config for llemacs-001 not found!
# requests for llemacs-001 not found!
# backups for llemacs-001 not found!
# Checking /workspace/llemacs-002 permissions...
# Workspace for llemacs-002 not found!
# src for llemacs-002 not found!
# logs for llemacs-002 not found!
# config for llemacs-002 not found!
# requests for llemacs-002 not found!
# backups for llemacs-002 not found!
# (A) root $ check_user_environment

# === Checking User Environment for llemacs-001 ===
# -bash: line 3: /opt/llemacs/src/shell/ENVS.sh.src: No such file or directory
# LLEMACS_LLM_API_KEY=
# LLEMACS_LLM_ENGINE=
# LLEMACS_ROOT_APPTAINER=/opt/llemacs
# LLEMACS_BASE_UID=9999
# LLEMACS_HOST_WORKSPACE=/opt/llemacs/config/env/../../workspace
# LLEMACS_GROUP=llemacs
# LLEMACS_ID=1
# LLEMACS_ROOT_HOST=/opt/llemacs/config/env/../..
# LLEMACS_ROOT=/opt/llemacs/config/env/../..
# LLEMACS_HOST_HOME=/home/llemacs-001
# LLEMACS_N_AGENTS=2
# LLEMACS_HOST_USER=llemacs-001
# Python venv: 
# /usr/bin/python
# (A) root $ 
# (A) root $ 
# EOF
