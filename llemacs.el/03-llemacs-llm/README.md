<!-- ---
!-- title: 2025-01-04 11:54:01
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/README.md
!-- --- -->

# 03-llemacs-llm
LLM integration with multiple provider support.

## Features
- Multiple LLM providers (Anthropic, Google, DeepSeek)
- API key management
- Rate limiting
- Error handling
- Prompt template system

## Components
### Core
- API configurations
- Provider implementations
- Call wrappers
- Error handlers

### Prompt System
- Template compilation
- Recipe management
- Component embedding

## Usage Examples
```elisp
;; Basic LLM calls
(llemacs-llm "Generate hello world")
(llemacs-llm "Plot sine wave" "code-gen")

;; Provider switching
(llemacs--llm-switch-provider "anthropic")
(llemacs--llm-switch-provider "google")

;; Template usage
(llemacs--llm-prompt-compile "code-gen")
(llemacs--llm-prompt-embed "hello" "code-gen")