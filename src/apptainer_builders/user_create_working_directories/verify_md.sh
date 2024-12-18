verify_md() {
    local md_file="$1"

    # Convert Markdown to JSON for verification
    local json_file="${md_file%.md}.json"
    if ! md2json.sh "$md_file" > "$json_file"; then
        echo "Error: Failed to convert Markdown to JSON: $md_file" >&2
        return 1
    fi

    # Verify the generated JSON file
    if ! verify_json_"$(basename "${md_file%.md}")"_template "$json_file"; then
        echo "Error: Markdown file $md_file is invalid (refer to JSON validation errors)" >&2
        rm "$json_file"
        return 1
    fi

    # Clean up the temporary JSON file
    rm "$json_file"

    echo "Markdown file $md_file is valid"
    return 0
}
