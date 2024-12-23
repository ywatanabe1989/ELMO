<!-- ---
!-- title: 2024-12-22 19:10:58
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.emacs.d/lisp/Ninja/config/env/README.md
!-- --- -->

# Ninja Environment Files

Environment configuration files for the Ninja project:

1. `00_all.env`: Sources all environment files in sequence
2. `01_path.env`: Project paths and directory structure
3. `02_base.env`: Basic system configuration and user settings
4. `03_container.env`: Container runtime and rsync configuration
5. `04_ninja_user.env`: Ninja agent user management and permissions
6. `05_external.env`: External API keys and service configurations

## Usage

Source all environment variables:
```bash
source /path/to/00_all.env
```