#!/bin/bash
# Time-stamp: "2024-12-20 01:22:54 (ywatanabe)"
# File: ./.dotfiles/.bash.d/all/030_github/github_basics.sh


parse_args() {
    local OPTIND
    TARGET_DIR="."
    while getopts ":t:" opt; do
        case $opt in
            t) TARGET_DIR="$OPTARG"
               ;;
            \?) echo "Invalid option -$OPTARG" >&2
               return 1
               ;;
        esac
    done
    shift $((OPTIND-1))
    ARGS=("$@")
}

gh_delete() {
    parse_args "$@"
    (
        cd "$TARGET_DIR" || exit
        gh repo delete
    )
}

gh_login() {
    gh auth login
}

gh_login_with_token() {
    gh auth login --with-token < $HOME/.bash.d/secrets/access_tokens/github.txt
}

gh_logout() {
    gh auth logout
}

gh_view() {
    parse_args "$@"
    (
        cd "$TARGET_DIR" || exit
        gh repo view | sed 's/\x1b\[[0-9;]*[mGKH]//g'
    )
}

gh_add_remote() {
    # Example: gh_add_remote origin ripple-wm-code
    local remote_name="$1"
    local repo_name="$2"

    if [ -z "$remote_name" ] || [ -z "$repo_name" ]; then
        echo "Usage: gh_add_remote <remote_name> <repo_name>"
        echo "Example: gh_add_remote origin user/repo"
        return 1
    fi

    # Get the authenticated user
    local user=$(gh api user -q .login)
    if [ -z "$user" ]; then
        echo "Error: Not authenticated. Please run gh_login first."
        return 1
    fi

    # If the repo_name doesn't include a '/', prepend the authenticated user
    if [[ "$repo_name" != *"/"* ]]; then
        repo_name="${user}/${repo_name}"
    fi

    # Check if the remote already exists
    if git remote get-url "$remote_name" &>/dev/null; then
        local existing_url=$(git remote get-url "$remote_name")
        read -p "Remote '$remote_name' already exists ($existing_url). Do you want to override it? (y/N): " confirm
        if [[ $confirm != [yY] ]]; then
            echo "Operation cancelled."
            return 1
        fi
        git remote remove "$remote_name"
    fi

    # Add the remote
    git remote add "$remote_name" "git@github.com:${repo_name}.git"

    if [ $? -eq 0 ]; then
        echo "Remote '$remote_name' added successfully for repository '$repo_name'."
    else
        echo "Failed to add remote '$remote_name' for repository '$repo_name'."
        return 1
    fi

    git remote -v
}

gh_create() {
    parse_args "$@"
    (
        cd "$TARGET_DIR" || exit
        gh repo create
    )
}

gh_rename() {
    parse_args "$@"
    (
        cd "$TARGET_DIR" || exit
        gh repo rename
    )
}

gh_clone() {
    parse_args "$@"

    ORIG_DIR=$(pwd)

    # Validate REPOSITORY_NAME
    REPOSITORY_NAME=${ARGS[0]}
    if [ -z "$REPOSITORY_NAME" ]; then
        echo "Error: Repository name is required."
        return 1
    fi

    # Check if the GITHUB_URL is properly formatted
    if [[ $GITHUB_URL == git@* || $GITHUB_URL == https://* ]]; then
        cd "$TARGET_DIR" || exit
        gh repo clone "$GITHUB_URL/$REPOSITORY_NAME.git"
    else
        cd "$TARGET_DIR" || exit
        gh repo clone "https://github.com/$GITHUB_URL/$REPOSITORY_NAME.git"
    fi

    cd "$ORIG_DIR"  # Return to the original directory
}


gh_protect_main() {
    parse_args "$@"
    (
        cd "$TARGET_DIR" || exit
        
        # Enable branch protection rules for main branch
        gh api \
           --method PUT \
           -H "Accept: application/vnd.github+json" \
           "/repos/{owner}/{repo}/branches/main/protection" \
           -f required_status_checks='{"strict":true,"contexts":[]}' \
           -f enforce_admins=true \
           -f required_pull_request_reviews='{"dismissal_restrictions":{},"dismiss_stale_reviews":true,"require_code_owner_reviews":true,"required_approving_review_count":1}' \
           -f restrictions=null
        
        echo "Main branch protection rules have been set:"
        echo "- Force pushes disabled"
        echo "- Branch deletion disabled" 
        echo "- Require pull request before merging"
        echo "- Require review before merging"
        echo "- Dismiss stale pull request approvals"
        echo "- Require status checks to pass before merging"
    )
}

# I often see this warnings on github.com:
# Your main branch isn't protected
# Protect this branch from force pushing or deletion, or require status checks before merging. View documentation.

# So, using the github client, please implement gh_protect_main function



# # to switch GitHub CLI to use the personal access token
# switchToPersonalGitHub() {
#     # Log out from the current GitHub CLI session
#     gh auth logout && \
    #         # Decrypt the personal GitHub token and write it to a temporary file
#         decript -t github_token_p > /tmp/_.txt && \
    #             # Log in to GitHub CLI using the personal access token from the temporary file
#         gh auth login --with-token < /tmp/_.txt && \
    #             # Remove the temporary file containing the token for security
#         rm -rf /tmp/_.txt
# }

# # to switch GitHub CLI to use the work access token
# switchToWorkGitHub() {
#     # Log out from the current GitHub CLI session
#     gh auth logout && \
    #         # Decrypt the work GitHub token and write it to a temporary file
#         decript -t github_token_w > /tmp/_.txt && \
    #             # Log in to GitHub CLI using the work access token from the temporary file
#         gh auth login --with-token < /tmp/_.txt && \
    #             # Remove the temporary file containing the token for security
#         rm -rf /tmp/_.txt
# }


# EOF
