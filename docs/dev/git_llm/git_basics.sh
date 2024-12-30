#!/bin/bash
# Time-stamp: "2024-12-19 11:27:07 (ywatanabe)"
# File: ./.dotfiles/.bash.d/all/030_git/git_basics.sh


parse_args() {
    local OPTIND
    TARGET_DIR="."
    MESSAGE=""
    FORCE=0
    FILES=()
    while getopts ":t:m:fh" opt; do
        case $opt in
            t) TARGET_DIR="$OPTARG" ;;
            m) MESSAGE="$OPTARG" ;;
            f) FORCE=1 ;;
            h) return 2 ;;
            \?) echo "Invalid option -$OPTARG" >&2; return 1 ;;
        esac
    done
    shift $((OPTIND-1))

    # Store remaining arguments as files
    FILES=("$@")

    [ -d "$TARGET_DIR" ] || { echo "Directory $TARGET_DIR not found" >&2; return 1; }
}



st() {
    usage_st() {
        echo "Usage: st [-t TARGET_DIR] [-h]"
        echo "Shows git status with file sizes"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  st"
        echo "  st -t ../project"
    }

    parse_args "$@"
    case $? in
        2) usage_st; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || { echo "Failed to change directory"; return 1; }
        git -c color.status=always status || { echo "Failed to get git status"; return 1; } | while read -r line; do
            if [[ $line =~ "modified:" ]] || [[ $line =~ "new file:" ]]; then
                file=$(echo "$line" | sed -E 's/^.*?(modified|new file):[[:space:]]*//' | sed 's/\x1b\[[0-9;]*m//g')
                if [ -e "$file" ]; then
                    if [ -L "$file" ]; then
                        target=$(readlink "$file") || target="unknown"
                        size_color=$CYAN
                        size="-> ${target}"
                    elif [ -d "$file" ]; then
                        size=$(du -sh "$file" 2>/dev/null | cut -f1) || size="unknown"
                        size_color=$PURPLE
                    else
                        bytes=$(stat -c%s "$file" 2>/dev/null) || bytes=0
                        size=$(numfmt --to=iec-i <<< "$bytes" 2>/dev/null)B || size="unknown"
                        if [ "${bytes:-0}" -gt 1048576 ]; then
                            size_color=$DARK_GRAY
                        else
                            size_color="\033[90m"
                        fi
                    fi
                    echo -e "$line ${size_color}(${size})\033[0m"
                else
                    echo -e "$line"
                fi
            else
                echo "$line"
            fi
        done
    )
}

alias br='git branch'
alias sw='git switch'
alias ch='git checkout'
alias fetch='git fetch'
alias push='git push'




ad() {
    usage_ad() {
        echo "Usage: ad [-t TARGET_DIR] [-m MESSAGE] [-h]"
        echo "Adds files to git staging area, handling symlinks specially"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -m MSG      Message (unused, for consistency)"
        echo "  -f          Force commit without confirmation"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  ad                     # Add all files"
        echo "  ad file1.txt dir/      # Add specific files"
        echo "  ad -t ../project       # Add all in different directory"
    }

    parse_args "$@"
    case $? in
        2) usage_ad; return 0 ;;
        1) return 1 ;;
    esac

    if [ ${#FILES[@]} -eq 0 ]; then
        (
            cd "$TARGET_DIR" || exit
            find scripts data -type l -exec git add -f {} + 2>/dev/null || true
            git add . || { echo "Failed to add files"; return 1; }
        )
    else
        for file in "${FILES[@]}"; do
            file=${file#./}
            if [ ! -e "$TARGET_DIR/$file" ] && [ ! -L "$TARGET_DIR/$file" ]; then
                echo "Warning: $file does not exist" >&2
                continue
            fi
            if [ -d "$TARGET_DIR/$file" ]; then
                git add -- "$TARGET_DIR/$file/"
            elif [ -L "$TARGET_DIR/$file" ]; then
                git add -f -- "$TARGET_DIR/$file"
            else
                git add -- "$TARGET_DIR/$file"
            fi
        done
    fi
}

cm() {
    usage_cm() {
        echo "Usage: cm [-t TARGET_DIR] [-m MESSAGE] [-h]"
        echo "Commits changes with a message or timestamp"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -m MSG      Commit message"
        echo "  -f          Force commit without confirmation"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  cm 'fix bug'"
        echo "  cm -m 'update readme'"
        echo "  cm -t ../project -m 'add feature'"
    }

    if ! command -v git >/dev/null 2>&1; then
        echo "Error: git is not installed" >&2
        return 1
    fi

    MESSAGE=""
    parse_args "$@"
    case $? in
        2) usage_cm; return 0 ;;
        1) return 1 ;;
    esac


    (
        if ! cd "$TARGET_DIR" 2>/dev/null; then
            echo "Error: Cannot change to directory: $TARGET_DIR" >&2
            return 1
        fi

        if ! git rev-parse --git-dir >/dev/null 2>&1; then
            echo "Error: Not a git repository" >&2
            return 1
        fi

        if git_list_unmerged_files | grep -q .; then
            printf "${RED}✗ Cannot commit: Unresolved conflicts detected${NC}\n" >&2
            git_list_conflicted_files
            return 1
        fi

        if [ ${#ARGS[@]} -eq 0 ] && [ -z "$MESSAGE" ]; then
            if ! genai_commit ${FORCE:+-f}; then
                echo "Commit cancelled" >&2
                return 1
            fi
        else
            commit_msg="${MESSAGE:-${ARGS[*]}}"
            git commit -m "$commit_msg"
        fi

    )
}

ac() {
    usage_ac() {
        echo "Usage: ac [-t TARGET_DIR] [-m MESSAGE] [-h] [files...]"
        echo "Adds specified files (or all changes) and commits with a message"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -m MSG      Commit message"
        echo "  -f          Force commit without confirmation"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  ac 'initial commit'"
        echo "  ac -m 'update readme' file1.txt file2.txt"
        echo "  ac -t ../project -m 'add feature' src/*.js"
    }

    parse_args "$@"
    case $? in
        2) usage_ac; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || exit
        if git_list_unmerged_files | grep -q .; then
            printf "${RED}✗ Cannot commit: Unresolved conflicts detected${NC}\n"
            git_list_conflicted_files
            return 1
        fi

        if [ ${#FILES[@]} -eq 0 ]; then
            git add .
        else
            git add "${FILES[@]}" || return 1
        fi

        if [ -z "$MESSAGE" ]; then
            if ! genai_commit ${FORCE:+-f}; then
                echo "Commit cancelled" >&2
                return 1
            fi
        else
            git commit -m "$commit_msg"
        fi
    )
}

acp() {
    usage_acp() {
        echo "Usage: acp [-t TARGET_DIR] [-m MESSAGE] [-h]"
        echo "Adds all changes, commits, and pushes to remote"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -m MSG      Commit message"
        echo "  -f          Force commit without confirmation"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  acp 'initial commit'"
        echo "  acp -m 'update readme'"
        echo "  acp -t ../project -m 'add feature'"
    }

    parse_args "$@"
    case $? in
        2) usage_acp; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || exit
        # Add conflict check
        if git_list_unmerged_files | grep -q .; then
            printf "${RED}✗ Cannot commit: Unresolved conflicts detected${NC}\n"
            git_list_conflicted_files
            return 1
        fi
        ad . || return 1
        if [ -z "$MESSAGE" ]; then
            if ! genai_commit ${FORCE:+-f}; then
                echo "Commit cancelled" >&2
                return 1
            fi
        else
            git commit -m "${MESSAGE:-${ARGS[*]}}"
        fi
        git push || { echo "Push failed"; return 1; }
    )
}

acpl() {
    usage_acpl() {
        echo "Usage: acpl [-t TARGET_DIR] [-m MESSAGE] [-h]"
        echo "Adds, commits, pushes and creates pull request"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -m MSG      Commit message"
        echo "  -f          Force commit without confirmation"
        echo "  -h          Show this help message"
        echo
        echo "Examples:"
        echo "  acpl 'feature complete'"
        echo "  acpl -m 'ready for review'"
    }

    parse_args "$@"
    case $? in
        2) usage_acpl; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || exit
        # Add conflict check
        if git_list_unmerged_files | grep -q .; then
            printf "${RED}✗ Cannot commit: Unresolved conflicts detected${NC}\n"
            git_list_conflicted_files
            return 1
        fi
        ad . || return 1

        if [ -z "$MESSAGE" ]; then
            if ! genai_commit ${FORCE:+-f}; then
                echo "Commit cancelled" >&2
                return 1
            fi
        else
            git commit -m "${MESSAGE:-${ARGS[*]}}"
        fi
        git push || { echo "Push failed"; return 1; }
        gh_pull_request || { echo "Pull request creation failed"; return 1; }        
        # gh_pull_request_from_develop_to_main || { echo "Pull request creation failed"; return 1; }
    )
}

pull() {
    usage_pull() {
        echo "Usage: pull [-t TARGET_DIR] [-h]"
        echo "Pulls changes from remote without edit prompt"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -h          Show this help message"
    }

    parse_args "$@"
    case $? in
        2) usage_pull; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || exit
        git pull --no-edit || { echo "Pull failed"; return 1; }
    )
}

log() {
    usage_log() {
        echo "Usage: log [-t TARGET_DIR] [-h]"
        echo "Shows formatted git log"
        echo
        echo "Options:"
        echo "  -t DIR      Target directory (default: current)"
        echo "  -h          Show this help message"
    }

    parse_args "$@"
    case $? in
        2) usage_log; return 0 ;;
        1) return 1 ;;
    esac

    (
        cd "$TARGET_DIR" || exit
        git log \
            --graph \
            --date=human \
            --decorate=short \
            --pretty=format:'%Cgreen%h %Creset%cd %Cblue%cn %Cred%d %Creset%s' || { echo "Log failed"; return 1; }
    )
}

# EOF
