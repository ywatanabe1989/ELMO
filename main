#!/bin/bash
# Time-stamp: "2024-12-30 17:10:24 (ywatanabe)"
# File: ./ELMO/run.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
    echo "Usage: $0 [-m|--mode <run|build|shell>] [-h|--help]"
    echo
    echo "Options:"
    echo "  -m, --mode   Operation mode (run|build|shell) (default: run)"
    echo "  -i, --init   Kill existing emacs daemon servers"
    echo "  -h, --help   Display this help message"
    echo
    echo "Example:"
    echo "  $0           # Run the container"
    echo "  $0 -m run    # Run the container"
    echo "  $0 -m build  # Build the container"
    echo "  $0 -m shell  # Enter shell in the container"
    exit 1
}

# Set default mode
mode="run"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -m|--mode)
            mode="$2"
            shift 2
            ;;
        -i|--init)
            pkill -f "emacs --daemon=/home/llemacs"
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            # Store remaining args for exec mode
            if [ "$mode" = "exec" ]; then
                exec_args=("$@")
                break
            else
                echo "Unknown option: $1"
                usage
            fi
            ;;
    esac
done

# Execute based on mode
if [ -n "$LLEMACS_BIND" ]; then
    bind_opt="--bind $LLEMACS_BIND"
else
    bind_opt=""
fi

source ./config/env/00_all.env

if [ "$mode" = "build" ]; then
    # export LLEMACS_HOME=. && source ./config/env/00_all.env
    unset APPTAINER_BIND
    bind_opt=""
    apptainer build \
              --sandbox \
              --fakeroot \
              $bind_opt \
              ./apptainer/llemacs.sandbox \
              ./apptainer/llemacs.def \
              2>&1 | tee ./apptainer/llemacs.sandbox.log

elif [ "$mode" = "run" ]; then
    # export LLEMACS_HOME=/opt/llemacs && source ./config/env/00_all.env
    apptainer run \
              --writable \
              --fakeroot \
              $bind_opt \
              ./apptainer/llemacs.sandbox

    # --userns \
        #     --bind $HOME:/home/$USER \


elif [ "$mode" = "shell" ]; then
    # export LLEMACS_HOME=/opt/llemacs && source ./config/env/00_all.env
    apptainer shell \
              --writable \
              --fakeroot \
              --shell "/bin/bash --login" \
              $bind_opt \
              ./apptainer/llemacs.sandbox

    # --bind $HOME:/home/$USER --userns

elif [ "$mode" = "exec" ]; then
    # export LLEMACS_HOME=/opt/llemacs && source ./config/env/00_all.env
    apptainer exec \
              --writable \
              --fakeroot \
              --cleanenv \
              $bind_opt \
              ./apptainer/llemacs.sandbox "${exec_args[@]}"

else
    echo "Invalid mode. Use run, build, or shell"
    usage

fi

# EOF
