#!/bin/bash

# Function to convert JSON to MD for agent template
json2md_agent_template() {
    local json_file="$1"
    local md_file="${json_file%.json}.md"

    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi

    local title=$(jq -r ".title" "$json_file")
    local tags=$(jq -r ".available_tags[]" "$json_file" | tr '\n' ',' | sed 's/,$//')
    local role=$(jq -r ".role" "$json_file")
    local responsibilities=$(jq -r ".responsibilities[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
    local tools=$(jq -r ".available_tools[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
    local expertise=$(jq -r ".expertise[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
    local communication=$(jq -r ".communication_protocols" "$json_file")
    local authorities=$(jq -r ".authorities" "$json_file")
    local notes=$(jq -r ".additional_notes" "$json_file")

    echo "---
title: \"$title\"
available tags: [$tags]
---

# Agent Template: $title

## **Role**

* **Primary Role:** $role

## **Responsibilities**

$responsibilities

## **Available Tools**

$tools

## **Expertise**

$expertise

## **Communication Protocols**

* **Preferred Method:** $communication

## **Authorities**

$authorities

## **Additional Notes**

$notes
" > "$md_file"

    if [[ $? -ne 0 ]]; then
        echo "Error creating $md_file"
        return 1
    fi
}

# Function to convert JSON to MD for tool template
json2md_tool_template() {
    local json_file="$1"
    local md_file="${json_file%.json}.md"

    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi

    local title=$(jq -r ".title" "$json_file")
    local tags=$(jq -r ".available_tags[]" "$json_file" | tr '\n' ',' | sed 's/,$//')
    local description=$(jq -r ".description" "$json_file")
    local elisp_command=$(jq -r ".elisp_command" "$json_file")
    local examples=$(jq -r ".examples" "$json_file")

    echo "---
title: \"$title\"
available tags: [$tags]
---

# Tool: $title

## **Description**

$description

## **Usage**

## **Elisp Command**
\`\`\`emacs-lisp
$elisp_command
\`\`\`

## **Examples**

$examples
" > "$md_file"

    if [[ $? -ne 0 ]]; then
        echo "Error creating $md_file"
        return 1
    fi
}

# Function to convert JSON to MD for prompt template (similar logic applies)
json2md_prompt_template() {
    local json_file="$1"
    local md_file="${json_file%.json}.md"

    if [ ! -f "$json_file" ]; then
        echo "Error: JSON file not found: $json_file" >&2
        return 1
    fi

    if ! command -v jq &> /dev/null; then
        echo "Error: jq is required. Please install it (e.g., 'apt-get install jq' or 'brew install jq')." >&2
        return 1
    fi

    local title=$(jq -r ".title" "$json_file")
    local tags=$(jq -r ".available_tags[]" "$json_file" | tr '\n' ',' | sed 's/,$//')
    local background=$(jq -r ".background" "$json_file")
    local requests=$(jq -r ".requests" "$json_file")
    local tools=$(jq -r ".tools" "$json_file")
    local data=$(jq -r ".data" "$json_file")
    local queue=$(jq -r ".queue" "$json_file")
    local expected_output=$(jq -r ".expected_output" "$json_file")
    local output_format=$(jq -r ".output_format" "$json_file")
    local additional_context=$(jq -r ".additional_context" "$json_file")

    echo "---
title: \"$title\"
available tags: [$tags]
---

# $title

## Background

$background

## Requests

$requests

## Tools (optional)

$tools

## Data (optional)

$data

## Queue (optional)

$queue

## **Expected Output**

$expected_output

## **Output Format**

$output_format

## **Additional Context**

$additional_context
" > "$md_file"

    if [[ $? -ne 0 ]]; then
        echo "Error creating $md_file"
        return 1
    fi
}

# Check if arguments are provided and determine template type
if [ $# -eq 1 ]; then
    local template_type=$(basename "$1" .json)

    case "$template_type" in
        "agent_profile")
            json2md_agent_template "$1"
            ;;
        "tool")
            json2md_tool_template "$1"
            ;;
        "prompt")
            json2md_prompt_template "$1"
            ;;
        *)
            echo "Error: Unsupported template type: $template_type" >&2
            exit 1
            ;;
    esac
else
    echo "Usage: $0 <json_template_file>"
    exit 1
fi

exit 0
