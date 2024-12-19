#!/bin/bash
# Time-stamp: "2024-12-19 15:13:17 (ywatanabe)"
# File: ./Ninja/workspace/formats/json2md.sh
# Function to print help message

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
    dir=$(dirname "$0")

    # Remove comment lines of the input file
    echo $input
    echo $output    

    python "$dir/json2md.py" "$input" > "$output"
}


md2json() {
    local input=$1
    local output=$2

    # Remove comment lines from markdown file
    local input_cleaned=$(mktemp)
    sed -e '/<!--/,/-->/d' -e '/^;/d' "$input" > "$input_cleaned"
    
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
