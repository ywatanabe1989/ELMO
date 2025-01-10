<!-- ---
!-- Timestamp: 2025-01-11 09:54:08
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/docs/installation.md
!-- --- -->

## Llemacs Installation

### 1. Elisp Setup
``` elisp
(load-file (expand-file-name "/path/to/llemacs/llemacs.el/llemacs.el"))
(require 'llemacs)
```
### 2. Python Environment

``` bash
python -m venv ./workspace/.env
source ./workspace/.env/bin/activate
python -m pip install -r requirements.txt
```

### 3. Environment Variables
Details can be seen at [`./config/env/`](./config/env/).
For example, LLM API keys are expected as follows:
```bash
# LLM
export LLEMACS_ANTHROPIC_API_KEY="your-key" # Default: $ANTHROPIC_API_KEY
export LLEMACS_GOOGLE_API_KEY="your-key" # Default: $GOOGLE_API_KEY
export LLEMACS_DEEPSEEK_API_KEY="your-key" # Default: $DEEPSEEK_API_KEY

# Git
export LLEMACS_GIT_EMAIL="your-agent-email-address"
export LLEMACS_GIT_USER_NAME="your-agent-name"
export LLEMACS_GIT_GITIGNORE_PATH="dot-gitignore-path"
```
`(llemacs-llm-switch-provider)` switches the LLM provider to use.

### 4. Mermaid
Please see [`./docs/installation/mermaid.md`](./docs/installation/mermaid.md).
