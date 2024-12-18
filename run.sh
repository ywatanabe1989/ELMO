#!/bin/bash
# Time-stamp: "2024-12-18 22:29:58 (ywatanabe)"
# File: ./Ninja/run.sh

pkill -f "emacs --daemon=/home/ninja"

apptainer run \
          --writable \
          --fakeroot \
          ./.apptainer/ninja/ninja.sandbox

# EOF

