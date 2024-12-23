#!/bin/bash
# Time-stamp: "2024-12-23 21:59:49 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/apptainer/utils/rename_ninja_to_elmo.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Create backup
timestamp=$(date +%Y%m%d_%H%M%S)
tar -czf backup_before_rename_${timestamp}.tar.gz .

# Search and replace in file contents
rg ninja -l | grep -v "$(basename $0)" | xargs sed -i.bak 's/ninja/elmo/g'
rg Ninja -l | grep -v "$(basename $0)" | xargs sed -i.bak 's/Ninja/ELMO/g'

# Rename files and directories
find . -depth -name '*[Nn]inja*' -not -name "$(basename $0)" -execdir bash -c 'mv "$1" "${1//[Nn]inja/elmo}"' _ {} \;
# EOF
