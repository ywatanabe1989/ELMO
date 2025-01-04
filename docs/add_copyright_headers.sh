#!/bin/bash

# Add GPL-3.0 headers to all .el files
find . -type f -name "*.el" -exec sh -c '
  if ! grep -q "Copyright (C)" "$1"; then
    temp=$(mktemp)
    echo ";; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)" > "$temp"
    echo ";;" >> "$temp"
    echo ";; This program is free software: you can redistribute it and/or modify" >> "$temp"
    echo ";; it under the terms of the GNU General Public License as published by" >> "$temp" 
    echo ";; the Free Software Foundation, either version 3 of the License, or" >> "$temp"
    echo ";; (at your option) any later version." >> "$temp"
    echo ";;" >> "$temp"
    cat "$1" >> "$temp"
    mv "$temp" "$1"
  fi
' sh {} \;
