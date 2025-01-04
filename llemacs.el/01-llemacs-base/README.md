<!-- ---
!-- title: 2025-01-04 11:51:03
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/README.md
!-- --- -->

# 01-llemacs-base
Core functionality and configuration for LLEMACS.

## Components
### Buffer Management
- Buffer variables and custom groups
- Buffer display functions
- Buffer name management for system/project

### Path Management
- System paths configuration
- Project paths configuration
- Log paths configuration
- Lock system implementation

### Utility Functions
- Symbol listing
- Debug helpers
- Timestamp management
- MD/JSON loaders

## Usage Examples
```elisp
;; List all LLEMACS symbols
(llemacs-list "command")
(llemacs-list "variable") 
(llemacs-list "function")
(llemacs-list "group")

;; Buffer operations
(llemacs--buf-disp-main-sys)
(llemacs--buf-disp-logging-pj)

;; Path management 
(llemacs--path-pj-update)
(llemacs--path-create-log-paths-sys)