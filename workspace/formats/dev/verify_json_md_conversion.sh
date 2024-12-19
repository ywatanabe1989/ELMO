#!/bin/bash
# Time-stamp: "2024-12-19 15:15:50 (ywatanabe)"
# File: ./Ninja/workspace/formats/verify.sh

# 1) .json_orig -> .json -> .md -> .json
# 2) check if .json_orig and .json are identical

# Clean up previous conversion files
rm -f *json2md *md2json *.json_orig.json *.json_orig.md


# Step 1: Copy original JSON files
for f in *.json_orig; do
    base="${f%.json_orig}"
    cp "$f" "${base}.json"
done

# Step 2: Convert JSON to MD
for f in *.json; do
    ./json2md.sh "$f"
done

# Step 3: Convert MD back to JSON
for f in *.md; do
    ./json2md.sh "$f"
done

# # Step 4: Compare original and final JSON
# for f in *.json_orig; do
#     base="${f%.json_orig}"
#     if ! diff -w "$f" "${base}.json"; then
#         echo "Conversion failed for $f"
#     fi
# done
