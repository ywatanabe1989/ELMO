
#!/bin/bash
# Time-stamp: "2024-12-21 16:10:00 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/ELMO/src/shell/init_project.sh
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_DIR="$(cd "${THIS_DIR}/../../workspace" && pwd)"

usage() {
    echo "Usage: $0 [-p|--project <project-name>] [-h|--help]"
    echo "Options:"
    echo "  -p, --project    Project name"
    echo "  -h, --help       Display this help message"
    echo
    echo "Example:"
    echo "  $0 -p hello-project"
    exit 1
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -p|--project)
            PROJECTNAME="$2"
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

if [ -z "$PROJECTNAME" ]; then
    echo "Error: Project name is required"
    usage
fi

ORIG_DIR="$(pwd)"
cd "$WORKSPACE_DIR"

NEXT_ID=$(ls -d projects/[0-9][0-9][0-9]-* 2>/dev/null | sort -r | head -n1 | sed 's/.*\/\([0-9][0-9][0-9]\).*/\1/')
NEXT_ID=$((${NEXT_ID:-0} + 1))
NEXT_ID=$(printf "%03d" $NEXT_ID)

export PROJECTNAME
cp -r projects/000-PROJECTNAME projects/${NEXT_ID}-${PROJECTNAME}

find projects/${NEXT_ID}-${PROJECTNAME} -type f -not -path "*.env*" -exec sed -i "s/template/${PROJECTNAME}/g" {} +
find projects/${NEXT_ID}-${PROJECTNAME} -type f -not -path "*.env*" -exec sed -i "s/PROJECTNAME/${PROJECTNAME}/g" {} +

tree projects/*-${PROJECTNAME}
cd "$ORIG_DIR"
# EOF
