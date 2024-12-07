#!/bin/bash
# Time-stamp: "2024-12-05 02:55:03 (ywatanabe)"
# File: ./self-evolving-agent/src/start.sh

. /etc/profile.d/sea.sh
$SEA_EMACS_CLIENT -e '(kill-emacs)'
HOME=SEA_HOME $SEA_EMACS_BIN --daemon &
for _ in seq 30; do
    $SEA_EMACS_CLIENT -c -n
    sleep 1
    echo $_
done

# EOF
