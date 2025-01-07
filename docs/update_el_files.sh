#!/bin/bash
# Time-stamp: "2025-01-05 17:02:46 (ywatanabe)"
# File: /home/ywatanabe/proj/llemacs/docs/update_el_files.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"


SCRIPT="$THIS_DIR/_update_el_files.sh"
find . -type f -name "*.el" -exec $SCRIPT {} \;


# EOF
