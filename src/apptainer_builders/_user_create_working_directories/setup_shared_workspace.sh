#!/bin/bash
# Time-stamp: "2024-12-18 17:55:23 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/user_create_working_directories/setup_shared_workspace.sh

THIS_DIR="$(dirname $0)"

# Source all .sh.src files in the current directory
for script in "$THIS_DIR"/*.sh.src; do
    if [ -f "$script" ]; then #check if file exists before sourcing
        source "$script"
    fi
done

# Create initial workspace directories
mkdir -p shared/agents/templates shared/agents/configs shared/tools shared/prompts

#Create initial templates
if command -v create_templates >/dev/null 2>&1; then #check if create_templates command exists
    create_templates shared
else
    echo "Error: create_templates command not found."
fi

echo "Initial workspace and templates created in 'shared' directory."

exit 0


# EOF
