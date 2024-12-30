#!/usr/bin/env bash
# Script created on: 2024-06-25 00:36:00
# Script path: /home/ywatanabe/.dotfiles/.bash.d/all/030-git/git-encrypt.sh

git_encrypt_secrets() {
    git-crypt init
    git-crypt add-gpg-user ywatanabe@alumni.u-tokyo.ac.jp

    # Ensure this is a Git repository
    if [ ! -d .git ]; then
        echo "Error: Not a git repository."
        return 1
    fi

    # Check if .gitignore exists, create if not
    if [ ! -f .gitignore ]; then
        touch .gitignore
        echo "Created .gitignore file."
    fi
    echo .git-crypt/keys >> .gitignore

    # Check if .gitattributes exists, create if not
    if [ ! -f .gitattributes ]; then
        touch .gitattributes
        echo "Created .gitattributes file."
    fi

    ########################################
    # Temporary file for new content
    local temp_file=$(mktemp)

    # Header for the git-crypt section
    echo -e "### git-crypt-secrets start ###" > "$temp_file"

    # Find directories containing 'secrets' and append git-crypt attributes to temp file
    echo -e "\n# Directories" >> "$temp_file"
    find . -type d -name "*secret*" | while read -r dir; do
        echo "${dir}/** filter=git-crypt diff=git-crypt" >> "$temp_file"
    done

    echo -e "\n# Files" >> "$temp_file"
    find . -type f -name "*secret*" | while read -r file; do
        echo "${file} filter=git-crypt diff=git-crypt" >> "$temp_file"
    done

    # Footer for the git-crypt section
    echo >> "$temp_file"
    echo -e "### git-crypt-secrets end ###" >> "$temp_file"
    ########################################

    # Replace old git-crypt section in .gitattributes
    if grep -q "### git-crypt-secrets start ###" .gitattributes; then
        # Section exists, replace it
        # Remove the old section
        sed -i '/### git-crypt-secrets start ###/,/### git-crypt-secrets end ###/d' .gitattributes
        # Append new section
        cat "$temp_file" >> .gitattributes
    else
        # Section does not exist, append
        cat "$temp_file" >> .gitattributes
    fi

    # Remove unnecessary part of files
    sed -i 's|\./||' .gitattributes

    # Remove the temporary file
    rm "$temp_file"

    # Add and commit the updated .gitattributes file
    git add .gitattributes
    git commit -m "Encrypted secret files"

    # Print the status
    git-crypt status | grep "secret"
    git-crypt status > .git-crypt/encrypt-status.txt

    echo -e "\nSee .git-crypt/encrypt-status.txt"
}

# EOF
