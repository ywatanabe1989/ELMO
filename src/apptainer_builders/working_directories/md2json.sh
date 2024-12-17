#!/bin/bash
md2json_agent_template() {
    local md_file="$1"
    local json_file="${md_file%.md}.json"

    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi

    local title=$(grep "^title: " "$md_file" | sed 's/^title: "//;s/"$//')
    local tags=$(grep "^available tags: " "$md_file" | sed 's/^available tags: \[//;s/\]$//' | tr ',' ' ')
    local role=$(sed -n '/## \*\*Role\*\*$/,/## \*\*Available Tools\*\*/p' "$md_file" | sed '1d;$d' | grep -v "Responsibilities:" | sed 's/- //g' | tr '\n' ' ')
    local responsibilities=$(sed -n '/## \*\*Role\*\*$/,/## \*\*Available Tools\*\*/p' "$md_file" | sed '1d;$d' | grep "Responsibilities:" | sed 's/- //g; s/Responsibilities://' | tr '\n' ' ')
    local tools=$(sed -n '/## \*\*Available Tools\*\*$/,/## \*\*Expertise\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- //g' | tr '\n' ' ')
    local expertise=$(sed -n '/## \*\*Expertise\*\*$/,/## \*\*Communication Protocols\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- //g' | tr '\n' ' ')
    local communication=$(sed -n '/## \*\*Communication Protocols\*\*$/,/## \*\*Authorities\*\*/p' "$md_file" | sed '1d;$d' | sed 's/- **Preferred Method:** //g' | tr '\n' ' ')
    local authorities=$(sed -n '/## \*\*Authorities\*\*$/,/## \*\*Additional Notes\*\*/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local notes=$(sed -n '/## \*\*Additional Notes\*\*$/,$p' "$md_file" | sed '1d' | tr '\n' ' ')

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
}
md2json_tool_template() {
    local md_file="$1"
    local json_file="${md_file%.md}.json"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi
    local title=$(grep "^title: " "$md_file" | sed 's/^title: "//;s/"$//')
    local tags=$(grep "^available tags: " "$md_file" | sed 's/^available tags: \[//;s/\]$//' | tr ',' ' ')
    local description=$(sed -n '/## \*\*Description\*\*$/,/## \*\*Usage\*\*/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local elisp_command=$(sed -n '/## \*\*Elisp Command\*\*$/,/## \*\*Examples\*\*/p' "$md_file" | sed '1d;$d' | sed 's/```emacs-lisp//;s/```//' | tr '\n' ' ')
    local examples=$(sed -n '/## \*\*Examples\*\*$/,$p' "$md_file" | sed '1d' | sed 's/```emacs-lisp//;s/```//' | tr '\n' ' ')

    echo "{
    \"title\": \"$title\",
    \"available_tags\": [$(echo "$tags" | sed 's/ /", "/g' | sed 's/^/"/;s/$/"/')],
    \"description\": \"$description\",
    \"elisp_command\": \"$elisp_command\",
    \"examples\": \"$examples\"

  }" > "$json_file"
}
md2json_prompt_template() {
    local md_file="$1"
    local json_file="${md_file%.md}.json"
    if [ ! -f "$md_file" ]; then
        echo "Error: Markdown file not found: $md_file" >&2
        return 1
    fi
    local title=$(grep "^title: " "$md_file" | sed 's/^title: "//;s/"$//')
    local tags=$(grep "^available tags: " "$md_file" | sed 's/^available tags: \[//;s/\]$//' | tr ',' ' ')
    local background=$(sed -n '/## Background$/,/## Requests/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local requests=$(sed -n '/## Requests$/,/## Tools \(optional\)/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local tools=$(sed -n '/## Tools \(optional\)$/,/## Data \(optional\)/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local data=$(sed -n '/## Data \(optional\)$/,/## Queue \(optional\)/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local queue=$(sed -n '/## Queue \(optional\)$/,/## \*\*Expected Output\*\*/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local expected_output=$(sed -n '/## \*\*Expected Output\*\*$/,/## \*\*Output Format\*\*/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local output_format=$(sed -n '/## \*\*Output Format\*\*$/,/## \*\*Additional Context\*\*/p' "$md_file" | sed '1d;$d' | tr '\n' ' ')
    local additional_context=$(sed -n '/## \*\*Additional Context\*\*$/,$p' "$md_file" | sed '1d' | tr '\n' ' ')

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
}
