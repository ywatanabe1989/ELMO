<!-- ---
!-- title: 2025-01-04 11:53:50
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/README.md
!-- --- -->

# 02-llemacs-logging
Logging system with file-based storage.

## Core Features
- Multiple log levels: debug, info, warn, error, prompt, search
- System and project-specific logging
- Log rotation and cleanup
- Viewing interfaces

## Components
### Log Management
- File initialization
- Level threshold control
- Log entry formatting
- File rotation

### Log Operations
- Writing logs by level
- Reading log entries
- Buffer viewing
- Log maintenance

## Usage Examples
```elisp
;; Write logs
(llemacs--logging-log-debug-sys "Debug message")
(llemacs--logging-log-info-pj "Info message for the current project (= `llemacs--cur-pj`)")

;; View logs
(llemacs--logging-view-logs-by-level-sys 'info)
(llemacs--logging-view-all-logs)

;; Maintain logs
(llemacs--logging-rotate-logs)
(llemacs--logging-clear-old-logs 30)