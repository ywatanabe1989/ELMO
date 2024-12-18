#!/bin/bash
# Time-stamp: "2024-12-18 09:43:46 (ywatanabe)"
# File: ./Ninja/run.sh

pkill -f "emacs --daemon=/home/ninja-"
apptainer run \
          --writable \
          --fakeroot \
          ./.apptainer/ninja/ninja.sandbox

# EOF

