#!/bin/bash
# Time-stamp: "2024-12-18 04:57:30 (ywatanabe)"
# File: ./Ninja/run.sh

apptainer run \
          --writable \
          --fakeroot \
          ./.apptainer/ninja/ninja.sandbox

# EOF

