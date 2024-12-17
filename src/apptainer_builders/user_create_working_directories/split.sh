#!/bin/bash
# Time-stamp: "2024-12-17 07:15:12 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/working_directories/split.sh

THIS_DIR="$(dirname $0)"

split_script() {
    local script_name="working_directory_setup_shared_dir.sh"
    # local dir="./Ninja/src/apptainer_builders/"
    local dir=$THIS_DIR/
    local original_script="${dir}${script_name}"

    # Check if the original script exists
    if [ ! -f "$original_script" ]; then
        echo "Error: Cannot find the script at $original_script" >&2
        return 1
    fi

    # Create subdirectories for better organization
    mkdir -p "${dir}helpers"
    mkdir -p "${dir}templates"
    mkdir -p "${dir}converters"
    mkdir -p "${dir}validators"
    mkdir -p "${dir}initializers"

    # Helper functions
    echo '#!/bin/bash' > "${dir}helpers/ensure_sdir.sh"
    sed -n '/^ensure_sdir() {/,/^}/p' "$original_script" >> "${dir}helpers/ensure_sdir.sh"

    # Template functions
    echo '#!/bin/bash' > "${dir}templates/create_templates.sh"
    sed -n '/^create_agent_template() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"
    sed -n '/^create_agent_config() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"
    sed -n '/^create_tool_md_template() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"
    sed -n '/^create_prompt_template() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"
    sed -n '/^create_tool_json_template() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"
    sed -n '/^create_tool_schema_template() {/,/^}/p' "$original_script" >> "${dir}templates/create_templates.sh"

    # Converter functions
    echo '#!/bin/bash' > "${dir}converters/md2json.sh"
    sed -n '/^md2json_agent_template() {/,/^}/p' "$original_script" >> "${dir}converters/md2json.sh"
    sed -n '/^md2json_tool_template() {/,/^}/p' "$original_script" >> "${dir}converters/md2json.sh"
    sed -n '/^md2json_prompt_template() {/, /^}/p' "$original_script" >> "${dir}converters/md2json.sh"

    echo '#!/bin/bash' > "${dir}converters/json2md.sh"
    sed -n '/^json2md_agent_template() {/,/^}/p' "$original_script" >> "${dir}converters/json2md.sh"
    sed -n '/^json2md_tool_template() {/,/^}/p' "$original_script" >> "${dir}converters/json2md.sh"
    sed -n '/^json2md_prompt_template() {/,/^}/p' "$original_script" >> "${dir}converters/json2md.sh"

    # Validator functions
    echo '#!/bin/bash' > "${dir}validators/verify.sh"
    sed -n '/^verify_md_agent_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"
    sed -n '/^verify_json_agent_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"
    sed -n '/^verify_md_tool_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"
    sed -n '/^verify_json_tool_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"
    sed -n '/^verify_md_prompt_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"
    sed -n '/^verify_json_prompt_template() {/,/^}/p' "$original_script" >> "${dir}validators/verify.sh"

    # Initializer function
    echo '#!/bin/bash' > "${dir}initializers/setup_workspace.sh"
    sed -n '/^setup_workspace() {/,/^}/p' "$original_script" >> "${dir}initializers/setup_workspace.sh"

    echo "Scripts have been split and organized into the following directories:"
    echo "- helpers/"
    echo "- templates/"
    echo "- converters/"
    echo "- validators/"
    echo "- initializers/"
}

split_script


# EOF
