#!/bin/bash
# Time-stamp: "2025-01-02 04:04:16 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/llemacs/workspace/resources/scripts/json2md.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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
        source /home/ywatanabe/.env/bin/activate
        mdjson "$input"
        # local input_extension="${input##*.}"
        # local output

        # if [[ "$input_extension" == "json" ]]; then
        #     output="${input%.json}.md"
        #     mdjson "$input" # -o "$output"            
        # elif [[ "$input_extension" == "md" ]]; then
        #     output="${input%.md}.json"
        #     mdjson "$input" # -o "$output"                        
        # else
        #     echo "Error: Unsupported file extension: $input_extension" >&2
        #     continue
        # fi
    done
}

main "$@"


# EOF
