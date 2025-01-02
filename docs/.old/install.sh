#!/bin/bash
# Time-stamp: "2024-12-02 06:20:36 (ywatanabe)"
# File: ./self-evolving-agent/docs/install.sh

set -euo pipefail
LOG_FILE="setup-sea.log"
CONFIG_DIR="/home/sea/.config/sea"

usage() {
    echo "Usage: $0 [-u|--user ] [-h|--help]"
    echo
    echo "Options:"
    echo " -u, --user Main user to grant sea access (required)"
    echo " -h, --help Display this help message"
    echo
    echo "Example:"
    echo " $0 -u ywatanabe"
    exit 1
}

log_msg() {
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] $1" | tee -a "$LOG_FILE"
}

check_dependencies() {
    local deps=("git" "sudo" "emacs")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &>/dev/null; then
            log_msg "Error: Required dependency '$dep' is not installed"
            exit 1
        fi
    done
}

setup_sea_user() {
    local main_user=$1

    if ! id "$main_user" &>/dev/null; then
        log_msg "Error: User $main_user does not exist"
        exit 1
    fi

    if ! id "sea" &>/dev/null; then
        log_msg "Creating sea user..."
        sudo useradd -r -m -d /home/sea sea || {
            log_msg "Error: Failed to create sea user"
            exit 1
        }
        sudo chmod 755 /home/sea
    fi

    log_msg "Configuring groups..."
    sudo usermod -aG sea "$main_user"
    sudo usermod -aG "$main_user" sea

    sudo chown -R sea:sea /home/sea
    mkdir -p "$CONFIG_DIR"
}

setup_sea_git_config() {
    log_msg "Setting up git configuration for sea user..."

    sudo -u sea git config --global user.name "sea-bot"
    sudo -u sea git config --global user.email "sea-bot@example.com"
    sudo -u sea git config --global core.editor "gedit"
    sudo -u sea git config --global init.defaultBranch "main"

    local gitignore="/home/sea/.gitignore_global"
    cat << EOF | sudo -u sea tee "$gitignore" > /dev/null
*~
.DS_Store
.env
*.log
EOF

    sudo -u sea git config --global core.excludesfile "$gitignore"
    sudo chmod 644 "$gitignore"

    log_msg "Git configuration completed"
}

setup_github_token() {
    log_msg "Setting up GitHub token..."

    local token_file="$CONFIG_DIR/github-token"
    local token=""
    local default_token=""

    if [[ -f "$token_file" ]]; then
        default_token=$(cat "$token_file")
        masked_default_token="${default_token:0:4}...${default_token: -4}"
        read -p "Enter GitHub Personal Access Token (press Enter for $masked_default_token, 's' to skip): " -r token

        if [[ -z "$token" ]]; then
            log_msg "Keeping existing token"
            return 0
        elif [[ "$token" == "s" ]]; then
            log_msg "Skipping token setup"
            return 0
        fi
    else
        read -p "Enter GitHub Personal Access Token ('s' to skip): " -r token
        if [[ "$token" == "s" ]]; then
            log_msg "Skipping token setup"
            return 0
        fi
    fi

    # if [[ -f "$token_file" ]]; then
    #     default_token=$(sudo cat "$token_file")
    #     masked_default_token="${default_token:0:4}...${default_token: -4}"
    #     if masked_default_token:
    #     read -p "Enter GitHub Personal Access Token (press Enter $masked_default_token, 's' to skip): " -r token
    #     else:
    #     read -p "Enter GitHub Personal Access Token ('s' to skip): " -r token
    #     if [[ -z "$token" ]]; then
    #         log_msg "Keeping existing token"
    #         return 0
    #     elif [[ "$token" == "s" ]]; then
    #         log_msg "Skipping GitHub token setup"
    #         return 0
    #     fi
    # else
    #     read -p "Enter GitHub Personal Access Token (press Enter to skip): " -r token
    #     if [[ -z "$token" ]]; then
    #         log_msg "Skipping GitHub token setup"
    #         return 0
    #     fi
    # fi

    if [[ ${#token} -lt 40 ]]; then
        log_msg "Error: Invalid token length. Skipping token setup."
        return 1
    fi

    echo "$token" | sudo -u sea tee "$token_file" > /dev/null
    sudo chmod 600 "$token_file"
    log_msg "GitHub token saved to $token_file"
}

# setup_workspace() {
#     log_msg "Setting up workspace..."

#     local dirs=("workspace" "backups" "logs")
#     for dir in "${dirs[@]}"; do
#         sudo mkdir -p "/opt/sea/$dir"
#     done

#     sudo chown -R sea:sea /opt/sea
#     sudo chmod -R 2775 /opt/sea

#     sudo -u sea mkdir -p "$CONFIG_DIR"
#     sudo -u sea touch "$CONFIG_DIR/keys.el"
#     sudo chmod 600 "$CONFIG_DIR/keys.el"
# }


# Add this new function after setup_workspace:
setup_source_permissions() {
    local main_user=$1
    local source_dir="/home/$main_user/.dotfiles/.emacs.d/lisp/self-evolving-agent"

    log_msg "Setting up source directory permissions..."

    # Make directories writable for owner and group
    sudo chmod -R 775 "$source_dir"

    # Set group ownership to sea
    sudo chgrp -R sea "$source_dir"

    # Set SGID bit on directories to maintain group ownership
    sudo find "$source_dir" -type d -exec chmod g+s {} \;

    # Make files writable for owner and group
    sudo find "$source_dir" -type f -exec chmod 664 {} \;
}


# setup_codebase_access() {
#     local main_user=$1
#     local source_dir="/home/$main_user/.dotfiles/.emacs.d/lisp/self-evolving-agent"
#     local target_dir="/opt/sea/workspace/self-evolving-agent"

#     log_msg "Setting up codebase access..."

#     # Check if source directory exists
#     if [[ ! -d "$source_dir" ]]; then
#         log_msg "Error: Source directory $source_dir not found"
#         exit 1
#     fi

#     # Remove existing target if any
#     sudo rm -f "$target_dir"

#     # Create symlink with sudo as sea user
#     sudo -u sea ln -sf "$source_dir" "$target_dir" || {
#         log_msg "Error: Failed to create symlink"
#         exit 1
#     }

#     # Set appropriate permissions
#     sudo chmod -R g+r "$source_dir"
#     sudo chgrp -R sea "$source_dir"

#     # Make specific directories writable for sea user
#     local writable_dirs=("src" "docs")
#     for dir in "${writable_dirs[@]}"; do
#         if [[ -d "$source_dir/$dir" ]]; then
#             sudo chmod -R g+w "$source_dir/$dir"
#         fi
#     done

#     log_msg "Codebase access configured at $target_dir"
# }

# verify_setup() {
#     log_msg "Verifying setup..."

#     local checks=(
#         "/opt/sea"
#         "$CONFIG_DIR/keys.el"
#         "$CONFIG_DIR/github-token"
#         "/opt/sea/workspace/self-evolving-agent"
#     )

#     for check in "${checks[@]}"; do
#         if [[ ! -e "$check" ]]; then
#             log_msg "Error: $check not found"
#             exit 1
#         fi
#     done

#     ls -la /opt/sea
#     ls -la "$CONFIG_DIR"
#     groups sea
#     groups "$1"
# }

main() {
    local main_user=""

    while [[ $# -gt 0 ]]; do
        case $1 in
            -u|--user)
                main_user="$2"
                shift 2
                ;;
            -h|--help)
                usage
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    if [[ -z "$main_user" ]]; then
        echo "Error: User parameter is required"
        usage
    fi

    check_dependencies
    setup_sea_user "$main_user"
    # setup_workspace
    setup_github_token
    setup_sea_git_config
    # setup_source_permissions "$main_user"
    # setup_codebase_access "$main_user"
    verify_setup "$main_user"

    log_msg "Setup completed successfully"
}

main "$@" 2>&1 | tee "$LOG_FILE"

# EOF
