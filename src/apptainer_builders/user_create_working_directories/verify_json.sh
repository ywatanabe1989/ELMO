verify_json_agent_template() {
    local json_file="$1"
    if [[ ! -f "$json_file" || $(jq -e '.title == null' "$json_file") ]]; then
        echo "Error: JSON file $json_file is invalid (missing title)" >&2
        return 1
    fi

    local required_fields=(title available_tags role responsibilities tools expertise communication authorities additional_notes)
    for field in "${required_fields[@]}"; do
        if [[ $(jq -e ".${field} == null" "$json_file") ]]; then
            echo "Error: JSON file $json_file is invalid (missing field: $field)" >&2
            return 1
        fi
    done

    echo "JSON file $json_file is valid"
    return 0
}

verify_json_tool_template() {
    local json_file="$1"
    if [[ ! -f "$json_file" || $(jq -e '.title == null' "$json_file") ]]; then
        echo "Error: JSON file $json_file is invalid (missing title)" >&2
        return 1
    fi

    local required_fields=(title available_tags description elisp_command examples)
    for field in "${required_fields[@]}"; do
        if [[ $(jq -e ".${field} == null" "$json_file") ]]; then
            echo "Error: JSON file $json_file is invalid (missing field: $field)" >&2
            return 1
        fi
    done

    echo "JSON file $json_file is valid"
    return 0
}

verify_json_prompt_template() {
    local json_file="$1"
    if [[ ! -f "$json_file" || $(jq -e '.title == null' "$json_file") ]]; then
        echo "Error: JSON file $json_file is invalid (missing title)" >&2
        return 1
    fi

    local required_fields=(title available_tags background requests tools data queue expected_output output_format additional_context)
    for field in "${required_fields[@]}"; do
        if [[ $(jq -e ".${field} == null" "$json_file") ]]; then
            echo "Error: JSON file $json_file is invalid (missing field: $field)" >&2
            return 1
        fi
    done

    echo "JSON file $json_file is valid"
    return 0
}
