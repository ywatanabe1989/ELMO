<!-- ---
!-- title: 2024-12-31 18:04:30
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/README.md
!-- --- -->

# LLEMACS Logging System

## Overview
The logging system for Llemacs with both file-based and database storage options.

## Structure
```
02-llemacs-logging/
├── core/          # Core components
│   ├── core-db.el     # Database core functionality
│   ├── core-file.el   # File system core functionality
│   └── core-utils.el  # Shared utilities
├── db/            # Database logging
│   ├── getters.el     # Database query functions
│   ├── initializers.el # Database setup
│   ├── loggers.el     # Database logging functions
│   ├── maintainers.el # Database maintenance
│   └── viewers.el     # Log viewing interfaces
└── file/          # File-based logging
    ├── getters.el     # File reading functions
    ├── initializers.el # File system setup
    ├── loggers.el     # File logging functions
    ├── maintainers.el # Log rotation/cleanup
    └── viewers.el     # Log viewing interfaces
```

## Features
- Dual storage: SQLite database and text files
- SQLite & File: Viewing interfaces
- SQLite: Structured logging with project/milestone/task hierarchy
- File: Log rotation and maintenance
- File: Debug, info, warning, and error levels

## Configuration
```elisp
;; Customize log directory
(setq llemacs--path-logging-system-logs "/custom/path/to/logs")

;; Set log retention period (in days)
(setq llemacs--logging-retention-days 30)

## Log Format
Each log entry follows this format:
```plaintext
[TIMESTAMP] [LEVEL] [Optional Context] MESSAGE

Examples:
[2024-12-31 18:00:00] [INFO] : Basic message
[2024-12-31 18:00:00] [ERROR] [Project: proj1] [File: test.el] [Line: 42] : Detailed message
```

## Basic Usage

### Loading function for testing
```elisp
(defun load-temporally ()
    (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
        (load-file (format "%s/../01-llemacs-config.el" dir))
        (load-file (format "%s/../02-llemacs-logging.el" dir))
        (message (format "%s" dir)))
    )
(load-temporally)
```

### Logging Messages

```elisp
;; Log messages at different levels
(llemacs--logging-log-debug "Debug message: %s" "test")
(llemacs--logging-log-info "Info message: %s" "test")
(llemacs--logging-log-warn "Warning message: %s" "test")
(llemacs--logging-log-error "Error message: %s" "test")

;; Log with additional context
(llemacs--logging-log 'info "Custom message" 
                      :project-id "myproject" 
                      :file-path "test.el" 
                      :line-num 42)
```

### Viewing Logs

```elisp
;; View logs of specific level
(llemacs--logging-view-logs-by-level "info")

;; View all logs
(llemacs--logging-view-all-logs)
```

### Getting Logs

``` elisp
;; Get last N entries of specific level
(llemacs--logging-get-logs-by-level 'info 10)

;; Get last N entries from all levels
(llemacs--logging-get-all-logs 10)
```

### More Examples

```elisp
;; Testing log rotation
(progn
  (llemacs--logging-log-info "Test message before rotation")
  (llemacs--logging-rotate-logs)
  (llemacs--logging-log-info "Test message after rotation"))

;; Testing with various formats
(llemacs--logging-log-debug "Number: %d, String: %s" 42 "test")
(llemacs--logging-log-info "Complex: %S" '(a b c))
(llemacs--logging-log-warn "Float: %.2f" 3.14159)

;; Testing context information
(llemacs--logging-log 'error "Critical error"
                      :project-id "test-project"
                      :file-path "critical.el"
                      :line-num 100)

;; Testing maintenance
(progn
  (llemacs--logging-compress-old-logs 0)  ; Compress immediately
  (llemacs--logging-clear-old-logs 0))    ; Clear immediately

;; Testing sequential operations
(progn
  (llemacs--logging-log-info "Step 1")
  (llemacs--logging-log-info "Step 2")
  (llemacs--logging-get-logs-by-level 'info 2))

;; Testing error handling
(condition-case err
    (llemacs--logging-log 'invalid "Should fail")
  (error (llemacs--logging-log-error "Caught error: %S" err)))
  
;; View all logs
(llemacs--logging-view-all-logs)
```

## Troubleshooting
- If logs aren't being written, check directory permissions
- Use `llemacs--logging-system-files-init-if` to reinitialize log files
- Use `llemacs--logging-rotate-logs` if log files grow too large