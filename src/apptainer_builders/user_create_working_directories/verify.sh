#!/bin/bash
verify_md_agent_template() {
    local md_file="$1"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi

    local title=$(grep "^title: " "$md_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $md_file" >&2
        return 1
    fi
    local tags=$(grep "^available tags: " "$md_file")
    if [ -z "$tags" ]; then
        echo "Error: available tags not found in $md_file" >&2
        return 1
    fi

    local role=$(sed -n '/## \*\*Role\*\*$/,/## \*\*Available Tools\*\*/p' "$md_file")
    if [ -z "$role" ]; then
        echo "Error: role not found in $md_file" >&2
        return 1
    fi
    local tools=$(sed -n '/## \*\*Available Tools\*\*$/,/## \*\*Expertise\*\*/p' "$md_file")
    if [ -z "$tools" ]; then
        echo "Error: available tools not found in $md_file" >&2
        return 1
    fi
    local expertise=$(sed -n '/## \*\*Expertise\*\*$/,/## \*\*Communication Protocols\*\*/p' "$md_file")
    if [ -z "$expertise" ]; then
        echo "Error: expertise not found in $md_file" >&2
        return 1
    fi
    local communication=$(sed -n '/## \*\*Communication Protocols\*\*$/,/## \*\*Authorities\*\*/p' "$md_file")
    if [ -z "$communication" ]; then
        echo "Error: communication protocols not found in $md_file" >&2
        return 1
    fi
    local authorities=$(sed -n '/## \*\*Authorities\*\*$/,/## \*\*Additional Notes\*\*/p' "$md_file")
    if [ -z "$authorities" ]; then
        echo "Error: authorities not found in $md_file" >&2
        return 1
    fi
    local notes=$(sed -n '/## \*\*Additional Notes\*\*$/,$p' "$md_file")
    if [ -z "$notes" ]; then
        echo "Error: notes not found in $md_file" >&2
        return 1
    fi

    echo "Markdown file $md_file is valid"
    return 0
}
verify_json_agent_template() {
    local json_file="$1"
    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi
    local title=$(jq -e ".title" "$json_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $json_file" >&2
        return 1
    fi
    local tags=$(jq -e ".available_tags" "$json_file")
    if [ -z "$tags" ]; then
        echo "Error: available_tags not found in $json_file" >&2
        return 1
    fi
    local role=$(jq -e ".role" "$json_file")
    if [ -z "$role" ]; then
        echo "Error: role not found in $json_file" >&2
        return 1
    fi
    local responsibilities=$(jq -e ".responsibilities" "$json_file")
    if [ -z "$responsibilities" ]; then
        echo "Error: responsibilities not found in $json_file" >&2
        return 1
    fi
    local tools=$(jq -e ".available_tools" "$json_file")
    if [ -z "$tools" ]; then
        echo "Error: available_tools not found in $json_file" >&2
        return 1
    fi
    local expertise=$(jq -e ".expertise" "$json_file")
    if [ -z "$expertise" ]; then
        echo "Error: expertise not found in $json_file" >&2
        return 1
    fi
    local communication=$(jq -e ".communication_protocols" "$json_file")
    if [ -z "$communication" ]; then
        echo "Error: communication_protocols not found in $json_file" >&2
        return 1
    fi
    local authorities=$(jq -e ".authorities" "$json_file")
    if [ -z "$authorities" ]; then
        echo "Error: authorities not found in $json_file" >&2
        return 1
    fi
    local notes=$(jq -e ".additional_notes" "$json_file")
    if [ -z "$notes" ]; then
        echo "Error: additional_notes not found in $json_file" >&2
        return 1
    fi

    echo "JSON file $json_file is valid"
    return 0
}
verify_md_tool_template() {
    local md_file="$1"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi
    local title=$(grep "^title: " "$md_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $md_file" >&2
        return 1
    fi
    local tags=$(grep "^available tags: " "$md_file")
    if [ -z "$tags" ]; then
        echo "Error: available tags not found in $md_file" >&2
        return 1
    fi
    local description=$(sed -n '/## \*\*Description\*\*$/,/## \*\*Usage\*\*/p' "$md_file")
    if [ -z "$description" ]; then
        echo "Error: description not found in $md_file" >&2
        return 1
    fi
    local elisp_command=$(sed -n '/## \*\*Elisp Command\*\*$/,/## \*\*Examples\*\*/p' "$md_file")
    if [ -z "$elisp_command" ]; then
        echo "Error: elisp command not found in $md_file" >&2
        return 1
    fi
    local examples=$(sed -n '/## \*\*Examples\*\*$/,$p' "$md_file")
    if [ -z "$examples" ]; then
        echo "Error: examples not found in $md_file" >&2
        return 1
    fi
    echo "Markdown file $md_file is valid"
    return 0
}
verify_json_tool_template() {
    local json_file="$1"
    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi
    local title=$(jq -e ".title" "$json_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $json_file" >&2
        return 1
    fi
    local tags=$(jq -e ".available_tags" "$json_file")
    if [ -z "$tags" ]; then
        echo "Error: available_tags not found in $json_file" >&2
        return 1
    fi
    local description=$(jq -e ".description" "$json_file")
    if [ -z "$description" ]; then
        echo "Error: description not found in $json_file" >&2
        return 1
    fi
    local elisp_command=$(jq -e ".elisp_command" "$json_file")
    if [ -z "$elisp_command" ]; then
        echo "Error: elisp_command not found in $json_file" >&2
        return 1
    fi
    local examples=$(jq -e ".examples" "$json_file")
    if [ -z "$examples" ]; then
        echo "Error: examples not found in $json_file" >&2
        return 1
    fi
    echo "JSON file $json_file is valid"
    return 0
}
verify_md_prompt_template() {
    local md_file="$1"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi
    local title=$(grep "^title: " "$md_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $md_file" >&2
        return 1
    fi
    local tags=$(grep "^available tags: " "$md_file")
    if [ -z "$tags" ]; then
        echo "Error: available tags not found in $md_file" >&2
        return 1
    fi
    local background=$(sed -n '/## Background$/,/## Requests/p' "$md_file")
    if [ -z "$background" ]; then
        echo "Error: background not found in $md_file" >&2
        return 1
    fi
    local requests=$(sed -n '/## Requests$/,/## Tools \(optional\)/p' "$md_file")
    if [ -z "$requests" ]; then
        echo "Error: requests not found in $md_file" >&2
        return 1
    fi
    local tools=$(sed -n '/## Tools \(optional\)$/,/## Data \(optional\)/p' "$md_file")
    if [ -z "$tools" ]; then
        echo "Error: tools not found in $md_file" >&2
        return 1
    fi
    local data=$(sed -n '/## Data \(optional\)$/,/## Queue \(optional\)/p' "$md_file")
    if [ -z "$data" ]; then
        echo "Error: data not found in $md_file" >&2
        return 1
    fi
    local queue=$(sed -n '/## Queue \(optional\)$/,/## \*\*Expected Output\*\*/p' "$md_file")
    if [ -z "$queue" ]; then
        echo "Error: queue not found in $md_file" >&2
        return 1
    fi
    local expected_output=$(sed -n '/## \*\*Expected Output\*\*$/,/## \*\*Output Format\*\*/p' "$md_file")
    if [ -z "$expected_output" ]; then
        echo "Error: expected_output not found in $md_file" >&2
        return 1
    fi
    local output_format=$(sed -n '/## \*\*Output Format\*\*$/,/## \*\*Additional Context\*\*/p' "$md_file")
    if [ -z "$output_format" ]; then
        echo "Error: output_format not found in $md_file" >&2
        return 1
    fi
    local additional_context=$(sed -n '/## \*\*Additional Context\*\*$/,$p' "$md_file")
    if [ -z "$additional_context" ]; then
        echo "Error: additional_context not found in $md_file" >&2
        return 1
    fi
    echo "Markdown file $md_file is valid"
    return 0
}
verify_md_prompt_template() {
    local md_file="$1"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi
    local title=$(grep "^title: " "$md_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $md_file" >&2
        return 1
    fi
    local tags=$(grep "^available tags: " "$md_file")
    if [ -z "$tags" ]; then
        echo "Error: available tags not found in $md_file" >&2
        return 1
    fi
    local background=$(sed -n '/## Background$/,/## Requests/p' "$md_file")
    if [ -z "$background" ]; then
        echo "Error: background not found in $md_file" >&2
        return 1
    fi
    local requests=$(sed -n '/## Requests$/,/## Tools \(optional\)/p' "$md_file")
    if [ -z "$requests" ]; then
        echo "Error: requests not found in $md_file" >&2
        return 1
    fi
    local tools=$(sed -n '/## Tools \(optional\)$/,/## Data \(optional\)/p' "$md_file")
    if [ -z "$tools" ]; then
        echo "Error: tools not found in $md_file" >&2
        return 1
    fi
    local data=$(sed -n '/## Data \(optional\)$/,/## Queue \(optional\)/p' "$md_file")
    if [ -z "$data" ]; then
        echo "Error: data not found in $md_file" >&2
        return 1
    fi
    local queue=$(sed -n '/## Queue \(optional\)$/,/## \*\*Expected Output\*\*/p' "$md_file")
    if [ -z "$queue" ]; then
        echo "Error: queue not found in $md_file" >&2
        return 1
    fi
    local expected_output=$(sed -n '/## \*\*Expected Output\*\*$/,/## \*\*Output Format\*\*/p' "$md_file")
    if [ -z "$expected_output" ]; then
        echo "Error: expected_output not found in $md_file" >&2
        return 1
    fi
    local output_format=$(sed -n '/## \*\*Output Format\*\*$/,/## \*\*Additional Context\*\*/p' "$md_file")
    if [ -z "$output_format" ]; then
        echo "Error: output_format not found in $md_file" >&2
        return 1
    fi
    local additional_context=$(sed -n '/## \*\*Additional Context\*\*$/,$p' "$md_file")
    if [ -z "$additional_context" ]; then
        echo "Error: additional_context not found in $md_file" >&2
        return 1
    fi
    echo "Markdown file $md_file is valid"
    return 0
}
verify_json_prompt_template() {
    local json_file="$1"
    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi
    local title=$(jq -e ".title" "$json_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $json_file" >&2
        return 1
    fi
    local tags=$(jq -e ".available_tags" "$json_file")
    if [ -z "$tags" ]; then
        echo "Error: available_tags not found in $json_file" >&2
        return 1
    fi
    local background=$(jq -e ".background" "$json_file")
    if [ -z "$background" ]; then
        echo "Error: background not found in $json_file" >&2
        return 1
    fi
    local requests=$(jq -e ".requests" "$json_file")
    if [ -z "$requests" ]; then
        echo "Error: requests not found in $json_file" >&2
        return 1
    fi
    local tools=$(jq -e ".tools" "$json_file")
    if [ -z "$tools" ]; then
        echo "Error: tools not found in $json_file" >&2
        return 1
    fi
    local data=$(jq -e ".data" "$json_file")
    if [ -z "$data" ]; then
        echo "Error: data not found in $json_file" >&2
        return 1
    fi
    local queue=$(jq -e ".queue" "$json_file")
    if [ -z "$queue" ]; then
        echo "Error: queue not found in $json_file" >&2
        return 1
    fi
    local expected_output=$(jq -e ".expected_output" "$json_file")
    if [ -z "$expected_output" ]; then
        echo "Error: expected_output not found in $json_file" >&2
        return 1
    fi
    local output_format=$(jq -e ".output_format" "$json_file")
    if [ -z "$output_format" ]; then
        echo "Error: output_format not found in $json_file" >&2
        return 1
    fi
    local additional_context=$(jq -e ".additional_context" "$json_file")
    if [ -z "$additional_context" ]; then
        echo "Error: additional_context not found in $json_file" >&2
        return 1
    fi
    echo "JSON file $json_file is valid"
    return 0
}
verify_json_prompt_template() {
    local json_file="$1"
    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi
    local title=$(jq -e ".title" "$json_file")
    if [ -z "$title" ]; then
        echo "Error: title not found in $json_file" >&2
        return 1
    fi
    local tags=$(jq -e ".available_tags" "$json_file")
    if [ -z "$tags" ]; then
        echo "Error: available_tags not found in $json_file" >&2
        return 1
    fi
    local background=$(jq -e ".background" "$json_file")
    if [ -z "$background" ]; then
        echo "Error: background not found in $json_file" >&2
        return 1
    fi
    local requests=$(jq -e ".requests" "$json_file")
    if [ -z "$requests" ]; then
        echo "Error: requests not found in $json_file" >&2
        return 1
    fi
    local tools=$(jq -e ".tools" "$json_file")
    if [ -z "$tools" ]; then
        echo "Error: tools not found in $json_file" >&2
        return 1
    fi
    local data=$(jq -e ".data" "$json_file")
    if [ -z "$data" ]; then
        echo "Error: data not found in $json_file" >&2
        return 1
    fi
    local queue=$(jq -e ".queue" "$json_file")
    if [ -z "$queue" ]; then
        echo "Error: queue not found in $json_file" >&2
        return 1
    fi
    local expected_output=$(jq -e ".expected_output" "$json_file")
    if [ -z "$expected_output" ]; then
        echo "Error: expected_output not found in $json_file" >&2
        return 1
    fi
    local output_format=$(jq -e ".output_format" "$json_file")
    if [ -z "$output_format" ]; then
        echo "Error: output_format not found in $json_file" >&2
        return 1
    fi
    local additional_context=$(jq -e ".additional_context" "$json_file")
    if [ -z "$additional_context" ]; then
        echo "Error: additional_context not found in $json_file" >&2
        return 1
    fi
    echo "JSON file $json_file is valid"
    return 0
}
