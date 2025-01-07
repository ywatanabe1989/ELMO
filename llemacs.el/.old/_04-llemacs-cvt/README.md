<!-- ---
!-- title: 2025-01-04 11:54:37
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/README.md
!-- --- -->

# 04-llemacs-cvt
Conversion utilities for various formats and languages.

## Components
### Text Compression
- LLM-based text summarization
- Length control
- Context preservation

### MD/JSON Conversion
- Bidirectional MD/JSON conversion
- Format validation
- Error handling

### Language to Elisp
- Natural language to Elisp conversion
- Code block extraction
- Response parsing

## Usage Examples
```elisp
;; Text compression
(llemacs--cvt-compress-text "Long text..." 1024)

;; MD/JSON conversion
(llemacs--cvt-mdjson "file.md")
(llemacs--cvt-mdjson "file.json")

;; Language to Elisp
(llemacs--cvt-prompt2elisp "Create a function that says hello")
(llemacs-extract-elisp-blocks "```elisp\n(message \"hi\")\n```")