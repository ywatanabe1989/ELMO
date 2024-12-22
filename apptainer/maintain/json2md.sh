#!/bin/bash
# Time-stamp: "2024-12-19 16:23:52 (ywatanabe)"
# File: ./Ninja/workspace/formats/json2md.sh
# Function to print help message

# Create temporary Python script
JSON2MD_PYTHON=$(mktemp)
cat > "$JSON2MD_PYTHON" << 'EOF'
import json
import sys
import argparse

def json2md(obj, level=1):
    output = []
    if isinstance(obj, dict):
        for key, value in obj.items():
            if output:
                output.append("")
            output.append("#" * level + " " + str(key))
            if isinstance(value, (dict, list)):
                output.append(json2md(value, level + 1))
            else:
                output.append(str(value) + "\n")
    elif isinstance(obj, list):
        for item in obj:
            if isinstance(item, (dict, list)):
                output.append(json2md(item, level))
            else:
                output.append("* " + str(item))
    return "\n".join(filter(None, output))

def main():
    if len(sys.argv) != 2:
        print("Usage: <input.json>", file=sys.stderr)
        sys.exit(1)
    
    with open(sys.argv[1], 'r') as f:
        data = json.load(f)
    print(json2md(data))

if __name__ == "__main__":
    main()
EOF


print_help() {
    cat << EOF
Usage:
json2md <input.json>
md2json <input.md>
Description:
Convert between JSON and human-readable Markdown formats.
Output filename is automatically determined by changing extension.
Arguments:
input       Input file path
Options:
-h, --help  Show this help message
EOF
    exit 0
}

# Function to determine output filename
get_output_filename() {
    local input=$1
    local cmd=$2
    case "$cmd" in
        json2md)
            echo "${input%.json}.md"
            ;;
        md2json)
            echo "${input%.md}.json"
            ;;
    esac
}

json2md() {
    local input=$1
    local output=$2
    python "$JSON2MD_PYTHON" "$input" > "$output"
}

md2json() {
    local input=$1
    local output=$2

    # Remove comment lines
    local input_cleaned=$(mktemp)
    sed -e '/<!--/,/-->/d' -e '/^;/d' "$input" > "$input_cleaned"

    # Apply md_to_json
    md_to_json "$input_cleaned" > "$output"
    rm "$input_cleaned"
}

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                print_help
                ;;
            *)
                break
                ;;
        esac
        shift
    done

    # Check for required arguments
    if [[ $# -lt 1 ]]; then
        echo "Error: Input file required" >&2
        print_help
    fi

    # Process each input file
    for input in "$@"; do
        local input_extension="${input##*.}"
        local output
        
        if [[ "$input_extension" == "json" ]]; then
            output="${input%.json}.md"
            json2md "$input" "$output"
        elif [[ "$input_extension" == "md" ]]; then
            output="${input%.md}.json"
            md2json "$input" "$output"
        else
            echo "Error: Unsupported file extension: $input_extension" >&2
            continue
        fi
    done
}

main "$@"
