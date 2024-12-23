#!/bin/bash
# Time-stamp: "2024-12-23 22:07:59 (ywatanabe)"
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
            pkill -f "emacs --daemon=/home/elmo"
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
if [ -n "$ELMO_BIND" ]; then
    bind_opt="--bind $ELMO_BIND"
else
    bind_opt=""
fi

if [ "$mode" = "build" ]; then
    export ELMO_HOME=. && source ./config/env/00_all.env
    apptainer build \
              --sandbox \
              --fakeroot \
              $bind_opt \
              ./apptainer/elmo.sandbox \
              ./apptainer/elmo.def \
              2>&1 | tee ./apptainer/elmo.sandbox.log

elif [ "$mode" = "run" ]; then
    export ELMO_HOME=/opt/ELMO && source ./config/env/00_all.env
    apptainer run \
              --writable \
              --fakeroot \
              $bind_opt \
              ./apptainer/elmo.sandbox


elif [ "$mode" = "shell" ]; then
    export ELMO_HOME=/opt/ELMO && source ./config/env/00_all.env
    apptainer shell \
              --writable \
              --fakeroot \
              --shell "/bin/bash --login" \
              $bind_opt \
              ./apptainer/elmo.sandbox

elif [ "$mode" = "exec" ]; then
    export ELMO_HOME=/opt/ELMO && source ./config/env/00_all.env
    apptainer exec \
              --writable \
              --fakeroot \
              --cleanenv \
              $bind_opt \
              ./apptainer/elmo.sandbox "${exec_args[@]}"

else
    echo "Invalid mode. Use run, build, or shell"
    usage

fi

# EOF
