
#!/bin/bash

# Add headers to all .el files
find . -type f -name "*.el" -exec sh -c '
if ! grep -q "Copyright (C)" "$1"; then
temp=$(mktemp)
# Remove existing headers
sed '/^;;;/d; /^;;/d' "$1" > "$temp"
# Add new headers
echo ";;; -*- coding: utf-8; lexical-binding: t -*-" > "$temp"
echo ";;; Author: $(date +"%Y-%m-%d %H:%M:%S")" >> "$temp"
echo ";;; Time-stamp: <$(date +"%Y-%m-%d %H:%M:%S") (ywatanabe)>" >> "$temp"
echo ";;; File: $1" >> "$temp"
echo ";;; >> "$temp"
echo ";; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)" >> "$temp"
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

# #!/bin/bash



# ;;; -*- coding: utf-8; lexical-binding: t -*-
# ;;; Author: 2025-01-01 22:53:16
# ;;; Time-stamp: <2025-01-01 22:53:16 (ywatanabe)>
# ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/00_entry.el

# ;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
#    ;;
#    ;; This program is free software: you can redistribute it and/or modify
#    ;; it under the terms of the GNU General Public License as published by
#    ;; the Free Software Foundation, either version 3 of the License, or
#    ;; (at your option) any later version.


#       # Add GPL-3.0 headers to all .el files
#       find . -type f -name "*.el" -exec sh -c '
#   if ! grep -q "Copyright (C)" "$1"; then
#     temp=$(mktemp)
#     echo ";; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)" > "$temp"
#     echo ";;" >> "$temp"
#     echo ";; This program is free software: you can redistribute it and/or modify" >> "$temp"
#     echo ";; it under the terms of the GNU General Public License as published by" >> "$temp" 
#     echo ";; the Free Software Foundation, either version 3 of the License, or" >> "$temp"
#     echo ";; (at your option) any later version." >> "$temp"
#     echo ";;" >> "$temp"
#     cat "$1" >> "$temp"
#     mv "$temp" "$1"
#   fi
# ' sh {} \;
