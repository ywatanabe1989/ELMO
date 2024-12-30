#!/bin/bash
# Time-stamp: "2024-12-09 09:44:03 (ywatanabe)"
# File: ./.dotfiles/.bash.d/all/030_git/git_add_gitignore_template.sh

git_add_gitignore_template() {
    local GITIGNORE_DEST="$PWD/.gitignore"
    local GITIGNORE_SRC_DIR="$HOME/.git-templates/.gitignore-templates"
    local GITIGNORE_SRC_FILES=(
        "$GITIGNORE_SRC_DIR/Custom/Specific.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/Secrets.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/Development.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/MyEmacs.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/Apptainer.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/Django.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/LargeFiles.gitignore"
        "$GITIGNORE_SRC_DIR/Python.gitignore"
        "$GITIGNORE_SRC_DIR/Django.gitignore"
        "$GITIGNORE_SRC_DIR/TeX.gitignore"
        "$GITIGNORE_SRC_DIR/Global/Emacs.gitignore"
        "$GITIGNORE_SRC_DIR/Custom/SQL.gitignore"
    )

    # Ensure .gitignore exists
    if [[ ! -f "$GITIGNORE_DEST" ]]; then
        touch "$GITIGNORE_DEST"
        echo -e "\n.gitignore file was created at $GITIGNORE_DEST."
    else
        echo -e "\n.gitignore file already exists at $GITIGNORE_DEST."
    fi

    # Add the templates
    for GITIGNORE_SRC in "${GITIGNORE_SRC_FILES[@]}"; do
        local TAG="### Source: $GITIGNORE_SRC ###"

        # Check if the tag already exists in the .gitignore
        if ! grep -qF "$TAG" "$GITIGNORE_DEST"; then
            echo -e "\n$TAG\n$(cat "$GITIGNORE_SRC")" >> "$GITIGNORE_DEST"
            echo -e "\n$GITIGNORE_SRC has been added."
        fi
        # else
            # echo -e "\n$GITIGNORE_SRC is already included. Skipped."
    done

    # Create a .gitignore.strip file with whitespace removed
    sed 's/^[ \t]*//;s/[ \t]*$//' "$GITIGNORE_DEST" > "$GITIGNORE_DEST"_tmp
    # echo "Whitespace-stripped version created at $GITIGNORE_DEST"
    mv "$GITIGNORE_DEST"_tmp "$GITIGNORE_DEST"
}

# EOF
