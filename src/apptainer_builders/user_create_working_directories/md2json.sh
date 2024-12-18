#!/bin/bash

# Function to convert MD to JSON for agent template
md2json_agent_template() {
    local md_file="$1"
    local json_file="${md_file%.md}.json"

    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi

    if ! command -v grep &> /dev/null; then
        echo "Error: grep is required." >&2
        return 1
    fi
    if ! command -v sed &> /dev/null; then
        echo "Error: sed is required." >&2
        return 1
    fi
    if ! command -v tr &> /dev/null; then
        echo "Error: tr is required." >&2
        return 1
    fi

    local title=$(grep "^title: " "$md_file" | sed 's/^title: "//;s/"$//')
    local tags=$(grep "^available tags: " "$md_file" | sed 's/^available tags: \[//;s/\]$//' | tr ',' ' ')
    local role=$(sed -n '/## \*\*Role\*\*$/,/## \*\*Responsibilities\*\*/p' "$md_file" | sed '1d;$d' | tr -d '\n')
    local responsibilities=$(sed -n '/## \*\*Responsibilities\*\*$/,/## \*\*Available Tools\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- //g' | tr '\n' ' ')
    local tools=$(sed -n '/## \*\*Available Tools\*\*$/,/## \*\*Expertise\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- //g' | tr '\n' ' ')
    local expertise=$(sed -n '/## \*\*Expertise\*\*$/,/## \*\*Communication Protocols\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- //g' | tr '\n' ' ')
    local communication=$(sed -n '/## \*\*Communication Protocols\*\*$/,/## \*\*Authorities\*\*/p' "$md_file" | sed '1d;$d' | sed 's/\* **Preferred Method:** //g' | tr -d '\n')
    local authorities=$(sed -n '/## \*\*Authorities\*\*$/,/## \*\*Additional Notes\*\*/p' "$md_file" | sed '1d;$d' | tr -d '\n')
    local notes=$(sed -n '/## \*\*Additional Notes\*\*$/,$p' "$md_file" | sed '1d' | tr -d '\n')

    echo "{
    \"title\": \"$title\",
    \"available_tags\": [$(echo "$tags" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"role\": \"$role\",
    \"responsibilities\": [$(echo "$responsibilities" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"available_tools\": [$(echo "$tools" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"expertise\": [$(echo "$expertise" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"communication_protocols\": \"$communication\",
    \"authorities\": \"$authorities\",
    \"additional_notes\": \"$notes\"
  }" > "$json_file"
    if [[ $? -ne 0 ]]; then
        echo "Error creating $json_file"
        return 1
    fi
}

# Function to convert MD to JSON for tool template
md2json_tool_template() {
    local md_file="$1"
    local json_file="${md_file%.md}.json"

    # Input validation and command checks (combined for efficiency)
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi

    if ! command -v grep &> /dev/null || ! command -v sed &> /dev/null || ! command -v tr &> /dev/null; then
        echo "Error: grep, sed, and tr are required. Please install them." >&2
        return 1
    fi

    # Extract fields using more robust methods where possible
    local title=$(grep "^title: " "$md_file" | sed 's/^title: "//;s/"$//')
    local tags=$(grep "^available tags: " "$md_file" | sed 's/^available tags: \[//;s/\]$//' | tr ',' ' ')

    # Use awk for more reliable multi-line extraction and code block removal
    local description=$(awk '/## \*\*Description\*\*$/,/## \*\*Usage\*\*/ {if (NR>1 && !/## \*\*Usage\*\*/){gsub(/\n/,""); print}}' "$md_file")
    local elisp_command=$(awk '/## \*\*Elisp Command\*\*$/,/## \*\*Examples\*\*/ {if (NR>1 && !/## \*\*Examples\*\*/){gsub(/```emacs-lisp/,""); gsub(/```/,""); gsub(/\n/,""); print}}' "$md_file")
    local examples=$(awk '/## \*\*Examples\*\*$/ {f=1; next} f {gsub(/```emacs-lisp/,""); gsub(/```/,""); gsub(/\n/,""); print}' "$md_file")

    # Construct JSON with proper quoting and array formatting
    echo "{
    \"title\": \"$title\",
    \"available_tags\": [$(echo "$tags" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"description\": \"$description\",
    \"elisp_command\": \"$elisp_command\",
    \"examples\": \"$examples\"
  }" > "$json_file"

    if [[ $? -ne 0 ]]; then
        echo "Error creating $json_file" >&2
        return 1
    fi
}

# Function to convert MD to JSON for prompt template
md2json_prompt_template() {
  local md_file="<span class="math-inline">1"
local json\_file\="</span>{md_file%.md}.json"

  if [ ! -f "$md_file" ]; then
    echo "Error: Markdown file not found: <span class="math-inline">md\_file" \>&2
return 1
fi
if \! command \-v grep &\> /dev/null; then
echo "Error\: grep is required\." \>&2
return 1
fi
if \! command \-v sed &\> /dev/null; then
echo "Error\: sed is required\." \>&2
return 1
fi
if \! command \-v tr &\> /dev/null; then
echo "Error\: tr is required\." \>&2
return 1
fi
local title\=</span>(grep "^title: " "<span class="math-inline">md\_file" \| sed 's/^title\: "//;s/"</span>//')
  local tags=$(grep "^available tags: " "<span class="math-inline">md\_file" \| sed 's/^available tags\: \\\[//;s/\\\]</span>//' | tr ',' ' ')
  local background=<span class="math-inline">\(sed \-n '/\#\# Background</span>/,/## Requests/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
local requests\=</span>(sed -n '/## Requests$/,/## Tools \(optional\)/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
local tools\=</span>(sed -n '/## Tools \(optional\)$/,/## Data \(optional\)/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
local data\=</span>(sed -n '/## Data \(optional\)$/,/## Queue \(optional\)/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
                    local queue\=</span>(sed -n '/## Queue \(optional\)$/,/## \*\*Expected Output\*\*/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
                                         local expected\_output\=</span>(sed -n '/## \*\*Expected Output\*\*$/,/## \*\*Output Format\*\*/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
local output\_format\=</span>(sed -n '/## \*\*Output Format\*\*$/,/## \*\*Additional Context\*\*/p' "$md_file" | sed '1d;<span class="math-inline">d' \| tr \-d '\\n'\)
local additional\_context\=</span>(sed -n '/## \*\*Additional Context\*\*$/,$p' "$md_file" | sed '1d' | tr -d '\n')

  echo "{
     \"title\": \"$title\",
     \"available_tags\": [$(echo "$tags" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
     \"background\": \"$background\",
     \"requests\": \"$requests\",
     \"tools\": \"$tools\",
     \"data\": \"$data\",
     \"queue\": \"$queue\",
     \"expected_output\": \"$expected_output\",
     \"output_format\": \"$output_format\",
     \"additional_context\": \"$additional_context\"
 }" > "$json_file"
    if [[ $? -ne 0 ]]; then
        echo "Error creating $json_file"
        return 1
    fi
}

# Check if arguments are provided and determine template type
if [ $# -eq 1 ]; then
  local template_type=$(basename "$1" .md)

  case "$template_type" in
    "agent_profile")
      md2json_agent_template "$1"
      ;;
    "tool")
      md2json_tool_template "$1"
      ;;
    "prompt")
      md2json_prompt_template "$1"
      ;;
    *)
      echo "Error: Unsupported template type: $template_type" >&2
      exit 1
      ;;
  esac
else
  echo "Usage: $0 <markdown_template_file>"
  exit 1
fi

exit 0
