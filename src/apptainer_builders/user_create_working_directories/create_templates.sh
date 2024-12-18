#!/bin/bash
set -euo pipefail

# Common logging function
log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >&2
}

# Error handling
error() {
    log "ERROR: $*"
    exit 1
}

# Check dependencies
for cmd in jq; do
    command -v "$cmd" >/dev/null 2>&1 || error "$cmd is required"
done

# Cleanup function
cleanup() {
    local tmpfiles=("$@")
    rm -f "${tmpfiles[@]}"
}
trap cleanup EXIT

# Function to create a JSON template file
create_json_template() {
    local type="$1"
    local filename="$2"
    local json_content=""

    case "$type" in
        "agent_profile")
            json_content='{
        "title": "[Agent Name]",
        "available_tags": [],
        "role": "[Agent Role]",
        "responsibilities": [],
        "available_tools": [],
        "expertise": [],
        "communication_protocols": "",
        "authorities": "",
        "additional_notes": ""
      }'
            ;;
        "agent_config")
            json_content='{
        "agent_id": "000",
        "name": "Example Agent",
        "model": "gpt-4",
        "temperature": 0.7,
        "max_tokens": 2000
      }'
            ;;
        "prompt")
            json_content='{
        "title": "[Prompt Title]",
        "available_tags": [],
        "background": "",
        "requests": "",
        "tools": "",
        "data": "",
        "queue": "",
        "expected_output": "",
        "output_format": "",
        "additional_context": ""
      }'
            ;;
        "tool")
            json_content='{
        "tool_id": "000",
        "tool_name": "Example Tool",
        "description": "",
        "elisp_command": "",
        "available_tags": [],
        "input": {
          "type": "object",
          "properties": {},
          "required": []
        },
        "output": {
          "type": "object",
          "properties": {},
          "required": []
        }
      }'
            ;;
        "tool_schema")
            json_content='{
            "$schema": "http://json-schema.org/draft-07/schema#",
            "title": "Tool Schema",
            "description": "Schema for validating tools",
            "type": "object",
            "properties": {
              "tool_id": { "type": "string" },
              "tool_name": { "type": "string" },
              "description": { "type": "string" },
              "elisp_command": { "type": "string" },
              "input": { "$ref": "#/definitions/io" },
              "output": { "$ref": "#/definitions/io" }
            },
            "required": ["tool_id", "tool_name", "description", "elisp_command", "input", "output"],
            "definitions": {
              "io": {
                "type": "object",
                "properties": {
                  "type": { "type": "string", "enum": ["object"] },
                  "properties": { "type": "object" },
                  "required": { "type": "array", "items": { "type": "string" } }
                },
                "required": ["type", "properties", "required"]
              }
            }
          }'
            ;;
        *)
            error "Invalid template type: $type"
            ;;
    esac

    echo "$json_content" > "$filename" || error "Failed to create $filename"
    log "Created $filename"
}

# Function to convert JSON to MD
json2md_template() {
    local json_file="$1"
    local md_file="${json_file%.json}.md"
    validate_file "$json_file"
    local type=$(basename "$json_file" .json)
    local title=$(jq -r ".title // \"\"" "$json_file")
    local tags=$(jq -r ".available_tags[]" "$json_file" | tr '\n' ',' | sed 's/,$//')
    local content=""

    case "$type" in
        "agent_profile")
            local role=$(jq -r ".role // \"\"" "$json_file")
            local responsibilities=$(jq -r ".responsibilities[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
            local tools=$(jq -r ".available_tools[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
            local expertise=$(jq -r ".expertise[]" "$json_file" | tr '\n' '  - ' | sed 's/,$//')
            local communication=$(jq -r ".communication_protocols // \"\"" "$json_file")
            local authorities=$(jq -r ".authorities // \"\"" "$json_file")
            local notes=$(jq -r ".additional_notes // \"\"" "$json_file")
            content="---
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
"
            ;;
        "tool")
            local description=$(jq -r ".description // \"\"" "$json_file")
            local elisp_command=$(jq -r ".elisp_command // \"\"" "$json_file")
            local examples=$(jq -r ".examples // \"\"" "$json_file")
            content="---
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
"
            ;;
        "prompt")
            local background=$(jq -r ".background // \"\"" "$json_file")
            local requests=$(jq -r ".requests // \"\"" "$json_file")
            local tools=$(jq -r ".tools // \"\"" "$json_file")
            local data=$(jq -r ".data // \"\"" "$json_file")
            local queue=$(jq -r ".queue // \"\"" "$json_file")
            local expected_output=$(jq -r ".expected_output // \"\"" "$json_file")
            local output_format=$(jq -r ".output_format // \"\"" "$json_file")
            local additional_context=$(jq -r ".additional_context // \"\"" "$json_file")
            content="---
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
"
            ;;
        *)
            error "Unsupported template type: $type"
            ;;
    esac
    echo "$content" > "$md_file" || error "Error creating $md_file"
    log "Created $md_file"
}


# Validate File Function
validate_file() {
    local file="$1"
    if [[ ! -f "$file" ]]; then
        error "Error: File not found: $file"
    fi
}


# Create JSON templates
create_json_template "agent_profile" "agent_profile.json"
create_json_template "agent_config" "agent_config.json"
create_json_template "prompt" "prompt.json"
create_json_template "tool" "tool.json"
create_json_template "tool_schema" "tool_schema.json"

# Convert JSON to MD
json2md_template "agent_profile.json"
json2md_template "tool.json"
json2md_template "prompt.json"

exit 0

# #!/bin/bash

# # Function to create a JSON template file
# create_json_template() {
#     local type="$1"
#     local filename="$2"

#     local json_content=""

#     case "$type" in
#         "agent_profile")
#             json_content='{
#         "title": "[Agent Name]",
#         "available_tags": [],
#         "role": "[Agent Role]",
#         "responsibilities": [],
#         "available_tools": [],
#         "expertise": [],
#         "communication_protocols": "",
#         "authorities": "",
#         "additional_notes": ""
#       }'
#             ;;
#         "agent_config")
#             json_content='{
#         "agent_id": "000",
#         "name": "Example Agent",
#         "model": "gpt-4",
#         "temperature": 0.7,
#         "max_tokens": 2000
#       }'
#             ;;
#         "prompt")
#             json_content='{
#         "title": "[Prompt Title]",
#         "available_tags": [],
#         "background": "",
#         "requests": "",
#         "tools": "",
#         "data": "",
#         "queue": "",
#         "expected_output": "",
#         "output_format": "",
#         "additional_context": ""
#       }'
#             ;;
#         "tool")
#             json_content='{
#         "tool_id": "000",
#         "tool_name": "Example Tool",
#         "description": "",
#         "elisp_command": "",
#         "input": {
#           "type": "object",
#           "properties": {},
#           "required": []
#         },
#         "output": {
#           "type": "object",
#           "properties": {},
#           "required": []
#         }
#       }'
#             ;;
#         "tool_schema")
#             json_content='{
#             "$schema": "http://json-schema.org/draft-07/schema#",
#             "title": "Tool Schema",
#             "description": "Schema for validating tools",
#             "type": "object",
#             "properties": {
#               "tool_id": { "type": "string" },
#               "tool_name": { "type": "string" },
#               "description": { "type": "string" },
#               "elisp_command": { "type": "string" },
#               "input": { "$ref": "#/definitions/io" },
#               "output": { "$ref": "#/definitions/io" }
#             },
#             "required": ["tool_id", "tool_name", "description", "elisp_command", "input", "output"],
#             "definitions": {
#               "io": {
#                 "type": "object",
#                 "properties": {
#                   "type": { "type": "string", "enum": ["object"] },
#                   "properties": { "type": "object" },
#                   "required": { "type": "array", "items": { "type": "string" } }
#                 },
#                 "required": ["type", "properties", "required"]
#               }
#             }
#           }'
#             ;;
#         *)
#             echo "Error: Invalid template type: $type" >&2
#             return 1
#             ;;
#     esac

#     echo "$json_content" > "$filename"
#     if [[ $? -ne 0 ]]; then
#         echo "Error creating $filename"
#         return 1
#     fi
# }

# # Create JSON templates
# create_json_template "agent_profile" "agent_profile.json"
# create_json_template "agent_config" "agent_config.json"
# create_json_template "prompt" "prompt.json"
# create_json_template "tool" "tool.json"
# create_json_template "tool_schema" "tool_schema.json"

# # Create MD "aliases" (using md2json)
# if command -v md2json >/dev/null 2>&1; then #check if md2json exists
#     touch agent_profile.md
#     md2json agent_profile.md
#     touch agent_config.md
#     md2json agent_config.md
#     touch prompt.md
#     md2json prompt.md
#     touch tool.md
#     md2json tool.md
#     touch tool_schema.md
#     md2json tool_schema.md
# else
#     echo "Warning: md2json command not found. Skipping MD template creation."
# fi

# exit 0
