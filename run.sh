#!/bin/bash
# Time-stamp: "2024-12-19 08:25:23 (ywatanabe)"
# File: ./Ninja/run.sh

LOG_FILE="$0.log"

usage() {
    echo "Usage: $0 [-m|--mode <run|build|shell>] [-h|--help]"
    echo
    echo "Options:"
    echo "  -m, --mode   Operation mode (run|build|shell) (default: run)"
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
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Kill existing emacs daemon
pkill -f "emacs --daemon=/home/ninja"

# Execute based on mode
if [ "$mode" = "run" ]; then
    apptainer run \
              --writable \
              --fakeroot \
              ./.apptainer/ninja/ninja.sandbox
elif [ "$mode" = "build" ]; then
    apptainer build \
              --sandbox \
              --fakeroot \
              ./.apptainer/ninja/ninja.sandbox \
              ./.apptainer/ninja/ninja.def \
              2>&1 | tee ./.apptainer/ninja/ninja.sandbox.log
elif [ "$mode" = "shell" ]; then
    apptainer shell \
              --fakeroot \
              --writable \
              ./.apptainer/ninja/ninja.sandbox
else
    echo "Invalid mode. Use run, build, or shell"
    usage
fi

# EOF

# #!/bin/bash
# # Time-stamp: "2024-12-19 08:21:10 (ywatanabe)"
# # File: ./Ninja/run.sh

# pkill -f "emacs --daemon=/home/ninja"

# help
# parse args

# if mode="run"
#    apptainer run \
    #              --writable \
    #              --fakeroot \
    #              ./.apptainer/ninja/ninja.sandbox
#    elif mode == "build"
#         apptainer build \
    #                   --sandbox \
    #                   --fakeroot \
    #                   ./.apptainer/ninja/ninja.sandbox \
    #                   ./.apptainer/ninja/ninja.def \
    #                   2>&1 | tee ./.apptainer/ninja/ninja.sandbox.log
#    elif mode == "shell"
#         apptainer shell \
    #                   --fakeroot \
    #                   --writable \
    #                   ./.apptainer/ninja/ninja.sandbox

#         # EOF

