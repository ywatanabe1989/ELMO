#!/bin/bash
# Time-stamp: "2024-12-07 18:29:36 (ywatanabe)"
# File: ./self-evolving-agent/apptainer/start_emacs.sh

#!/bin/bash

emacsclient -e '(kill-emacs)' || true
emacs --daemon &
sleep 5

for i in {1..10}; do
    if emacsclient -e '(+ 1 2)' >/dev/null 2>&1; then
        emacsclient -c -n
        break
    fi
    sleep 1
done


# EOF
