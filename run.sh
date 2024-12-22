#!/bin/bash
# Time-stamp: "2024-12-23 02:13:02 (ywatanabe)"
# File: ./Ninja/run.sh

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
source $THIS_DIR/config/env/06_apptainerenv.env
export NINJA_ROOT=${NINJA_ROOT:-$THIS_DIR}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -m|--mode)
            mode="$2"
            shift 2
            ;;
        -i|--init)
            pkill -f "emacs --daemon=/home/ninja"
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
if [ -n "$NINJA_BIND" ]; then
    bind_opt="--bind $NINJA_BIND"
else
    bind_opt=""
fi

if [ "$mode" = "run" ]; then
    apptainer run \
              --writable \
              --fakeroot \
              $bind_opt \
              $NINJA_ROOT/apptainer/ninja.sandbox

elif [ "$mode" = "build" ]; then
    apptainer build \
              --sandbox \
              --fakeroot \
              $bind_opt \
              $NINJA_ROOT/apptainer/ninja.sandbox \
              $NINJA_ROOT/apptainer/ninja.def \
              2>&1 | tee $NINJA_ROOT/apptainer/ninja.sandbox.log
    # --update \    

elif [ "$mode" = "shell" ]; then
    apptainer shell \
              --writable \
              --fakeroot \
              $bind_opt \
              $NINJA_ROOT/apptainer/ninja.sandbox

    # In the exec section, use the stored args
elif [ "$mode" = "exec" ]; then
    apptainer exec \
              --writable \
              --fakeroot \
              $bind_opt \
              $NINJA_ROOT/apptainer/ninja.sandbox "${exec_args[@]}"

else
    echo "Invalid mode. Use run, build, or shell"
    usage

fi

# EOF
