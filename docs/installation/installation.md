<!-- ---
!-- Timestamp: 2025-01-11 18:14:20
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/docs/installation/installation.md
!-- --- -->

## Llemacs Installation

--------------------------------------------------------------------------------
### 1. Elisp Setup
--------------------------------------------------------------------------------

Add something like this in your Emacs init file or start a temporary Emacs session:

```elisp
(load-file (expand-file-name "/path/to/llemacs/llemacs.el"))
(require 'llemacs)
```

--------------------------------------------------------------------------------
### 2. Python Environment
--------------------------------------------------------------------------------

If you plan to run Python scripts or LLM providers that need Python-based dependencies:

```bash
python -m venv ./workspace/.env
source ./workspace/.env/bin/activate
pip install -r requirements.txt
```

--------------------------------------------------------------------------------
### 3. Environment Variables
--------------------------------------------------------------------------------

See [`./config/env/`](../../config/env/) for example env files. Make sure to export any required LLM keys:

```bash
# LLM
export LLEMACS_ANTHROPIC_API_KEY="your-anthropic-key"   # or use $ANTHROPIC_API_KEY
export LLEMACS_GOOGLE_API_KEY="your-google-key"         # or use $GOOGLE_API_KEY
export LLEMACS_DEEPSEEK_API_KEY="your-deepseek-key"     # or use $DEEPSEEK_API_KEY

# Git (optional)
export LLEMACS_GIT_EMAIL="your-agent-email@domain"
export LLEMACS_GIT_USER_NAME="AgentName"
export LLEMACS_GIT_GITIGNORE_PATH="/path/to/.gitignore"
```

You can switch LLM providers with:
```elisp
(llemacs-llm-switch-provider "google")  ; or "anthropic", etc.
```

--------------------------------------------------------------------------------
### 4. Mermaid
--------------------------------------------------------------------------------

For diagram generation, see [`./docs/installation/mermaid.md`](mermaid.md). You’ll need Node.js and mermaid-cli installed.
<!-- <\!-- ---
 !-- !-- Timestamp: 2025-01-11 17:45:15
 !-- !-- Author: ywatanabe
 !-- !-- File: /home/ywatanabe/proj/llemacs/docs/installation/installation.md
 !-- !-- --- -\->
 !-- 
 !-- ## Llemacs Installation
 !-- 
 !-- ### 1. Elisp Setup
 !-- ``` elisp
 !-- (load-file (expand-file-name "/path/to/llemacs/llemacs.el/llemacs.el"))
 !-- (require 'llemacs)
 !-- ```
 !-- ### 2. Python Environment
 !-- 
 !-- ``` bash
 !-- python -m venv ./workspace/.env
 !-- source ./workspace/.env/bin/activate
 !-- python -m pip install -r requirements.txt
 !-- ```
 !-- 
 !-- ### 3. Environment Variables
 !-- Details can be seen at [`./config/env/`](./config/env/).
 !-- For example, LLM API keys are expected as follows:
 !-- ```bash
 !-- # LLM
 !-- export LLEMACS_ANTHROPIC_API_KEY="your-key" # Default: $ANTHROPIC_API_KEY
 !-- export LLEMACS_GOOGLE_API_KEY="your-key" # Default: $GOOGLE_API_KEY
 !-- export LLEMACS_DEEPSEEK_API_KEY="your-key" # Default: $DEEPSEEK_API_KEY
 !-- 
 !-- # Git
 !-- export LLEMACS_GIT_EMAIL="your-agent-email-address"
 !-- export LLEMACS_GIT_USER_NAME="your-agent-name"
 !-- export LLEMACS_GIT_GITIGNORE_PATH="dot-gitignore-path"
 !-- ```
 !-- `(llemacs-llm-switch-provider)` switches the LLM provider to use.
 !-- 
 !-- ### 4. Mermaid
 !-- Please see [`./docs/installation/mermaid.md`](./docs/installation/mermaid.md). -->
