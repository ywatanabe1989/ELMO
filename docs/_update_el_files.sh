#!/bin/bash

filename=$1
echo "Updating $filename"
temp_file=$(mktemp)

# Remove all consecutive commenting lines at the beginning of the file
sed '/^;/,/^$/d' "$filename" > "$temp_file"

> "$filename"
# Insert the header
echo ";;; -*- coding: utf-8; lexical-binding: t -*-" >> "$filename"
echo ";;; Author: $(date +"%Y-%m-%d %H:%M:%S")" >> "$filename"
echo ";;; Time-stamp: <$(date +"%Y-%m-%d %H:%M:%S") (ywatanabe)>" >> "$filename"
echo ";;; File: $filename" >> "$filename"
echo ";;" >> "$filename"
echo ";; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)" >> "$filename"
echo ";;" >> "$filename"
echo ";; This program is free software: you can redistribute it and/or modify" >> "$filename"
echo ";; it under the terms of the GNU General Public License as published by" >> "$filename"
echo ";; the Free Software Foundation, either version 3 of the License, or" >> "$filename"
echo ";; (at your option) any later version." >> "$filename"

# Append the rest of the file
cat "$temp_file" >> "$filename"
rm "$temp_file"
