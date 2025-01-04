<!-- ---
!-- title: 2025-01-04 11:50:27
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/README.md
!-- --- -->

# /llemacs.el
Core Elisp package implementing LLEMACS (LLM Agents on Emacs).

## These commands will list commands/variables/functions/groups starting from ^llemacs- prefix.
``` elisp
(llemacs-list "command")
(llemacs-list "variable")
(llemacs-list "function")
(llemacs-list "group")
```

## Directory Structure
```
llemacs.el/
├── 01-llemacs-base/     # Core functionality & configuration
├── 02-llemacs-logging/  # Logging system 
├── 03-llemacs-llm/      # LLM integration 
├── 04-llemacs-cvt/      # Converters
├── 05-llemacs-run/      # Execution engine
├── 06-llemacs-proj/     # Project management
├── 07-llemacs-tools/    # Additional tools
└── 08-llemacs-git/      # Git integration
```

## Key files in each directory:

### 01-llemacs-base/
- `000-entry.el`: Entry point
- `000-list.el`: Symbol listing utilities  
- `001-groups.el`: Custom groups definitions
- `010-buf-var.el`: Buffer variables
- `011-buf-func.el`: Buffer functions
- `100-paths-*.el`: Path management
- `999-*.el`: Debug, timestamp etc.

### 02-llemacs-logging/
- `00-entry.el`: Entry point
- `variables.el`: Logging variables
- `getters.el`: Log retrieval
- `loggers.el`: Log writing functions
- `viewers.el`: Log viewing functions
- `maintainers.el`: Log maintenance

### 03-llemacs-llm/
- `00-entry.el`: Entry point  
- `core-*.el`: LLM integration core
- `prompt-*.el`: Prompt management

### 04-llemacs-cvt/
- `00-entry.el`: Entry point
- `t2t-compress.el`: Text compression
- `mdjson.el`: MD/JSON conversion
- `lang2elisp.el`: Language to Elisp

### 05-llemacs-run/
- `00-entry.el`: Entry point
- `run-elisp.el`: Elisp execution
- `run-prompt.el`: Prompt execution
- `helpers.el`: Helper functions

### 06-llemacs-proj/
- `00-entry.el`: Entry point
- `proj-init.el`: Project initialization
- `proj-variables.el`: Project variables
- `proj-collect-context.el`: Context collection

### 07-llemacs-tools/
- Search functionality

### 08-llemacs-git/
- Git operations implementation