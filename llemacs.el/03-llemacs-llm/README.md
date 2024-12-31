<!-- ---
!-- title: 2024-12-31 18:57:50
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/03-llemacs-llm/README.md
!-- --- -->

# LLEMACS LLM Module

## Overview
Llemacs LLM module provides integration with various LLM providers (DeepSeek, OpenAI, Anthropic, Google) for text processing capabilities in Emacs.

## Directory Structure
```
03-llemacs-llm/
├── core/
│   ├── llemacs-llm-api.el         # API interface and utilities
│   ├── llemacs-llm-providers.el   # LLM provider implementations
│   └── llemacs-llm-core.el        # Core LLM functionality
├── prompt/
│   ├── llemacs-llm-prompt-main.el # Prompt handling 
│   └── llemacs-llm-prompt-recipes.el # Prompt templates and recipes
└── resources/
    └── prompt-templates/
        ├── compiled/      # Compiled prompt templates
        └── components/    # Prompt component files
```

## Features
- Multiple LLM provider support
- API rate limiting and error handling
- Prompt template system
- Configurable via environment variables

## Configuration
Set environment variables for API keys:
- ANTHROPIC_API_KEY
- GOOGLE_API_KEY
- DEEPSEEK_API_KEY

## Usage Example
```elisp
(llemacs-llm "Your prompt" "template-name")
(llemacs-llm-switch-provider "deepseek")
```