#!/bin/bash

ensure_sdir() {
    local spath="$1"
    if [ -z "$spath" ]; then
        echo "Error: No path provided to ensure_sdir" >&2
        return 1
    fi
    mkdir -p "$(dirname "$spath")"
}
