<!-- Time-stamp: "2025-01-06 17:19:14 (ywatanabe)" -->
<!-- File: code-shell-script.md -->

# Example: code-shell-script
``` bash
#!/bin/bash
# script-name.sh
# Author: ywatanabe (ywatanabe@alumni.u-tokyo.ac.jp)
# Date: $(date +"%Y-%m-%d-%H-%M")

LOG_FILE=."$0.log" # Do not remove existing extension (e.g., script.sh.log is preferred)

usage() {
    echo "Usage: $0 [-s|--subject <subject>] [-m|--message <message>] [-h|--help]"
    echo-
    echo "Options:"
    echo "  -s, --subject   Subject of the notification (default: 'Subject')"
    echo "  -m, --message   Message body of the notification (default: 'Message')"
    echo "  -h, --help      Display this help message"
    echo
    echo "Example:"
    echo "  $0 -s "About the Project A" -m "Hi, ..."
    echo "  $0 -s "Notification" -m "This is a notification from ..."
    exit 1
}

my-echo() {
  while [[ $# -gt 0 ]]; do
      case $1 in
          -s|--subject)
              subject="$1"
              shift 1
              ;;
          -m|--message)
              shift
              message="$1"
              shift
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

  echo "${subject:-Subject}: ${message:-Message} (Yusuke Watanabe)"
}

main() {
    my-echo "$@"
}

main "$@" 2>&1 | tee "$LOG_FILE"

notify -s "$0 finished" -m "$0 finished"

# EOF
```
