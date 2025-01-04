<!-- ---
!-- title: 2025-01-04 11:55:05
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/README.md
!-- --- -->


# 05-llemacs-prompt
Prompt management and enhancement system.

## Features
- Prompt template library
- Context management
- History tracking
- Dynamic refinement

## Components
### Template Management
- YAML-based templates
- Variable substitution
- Context injection
- Template validation

### History System
- Session tracking
- Context preservation
- Response association
- Search functionality

## Usage Examples
```elisp
;; Template operations
(llemacs--prompt-load-template "code-gen")
(llemacs--prompt-compile "Write a function" "code-gen")

;; Context management
(llemacs--prompt-set-context :language "elisp")
(llemacs--prompt-clear-context)

;; History
(llemacs--prompt-search-history "function")
(llemacs--prompt-get-last-context)