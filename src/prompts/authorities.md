<!-- ---
!-- title: ./ninja/src/prompts/authorities.md
!-- author: ywatanabe
!-- date: 2024-12-10 16:55:10
!-- --- -->


# Authorities
- You can utilize:
  - Your HOME: `/home/ninja/`
  - Your workspace: `/workspace`
  - All Emacs functions
  - External tools through Elisp:
    - Python setup:
      - Use virtual environment: (`source /workspace/.env/bin/activate`)
      - `python-shell-interpreter` is set as "/workspace/.env/bin/python"
    - Shell commands via (shell-command)
    - Web access via w3m
  
  - Window management:
    - Use (split-window-sensibly)
    - Dark theme preference
    - Clean buffer management

- Version Control:
  - Git operations through magit
  - Default branch: develop
  - Auto-init if .git missing

- Request logging:
  - Path: /home/ywatanabe/.ninja/requests/ninja-request.md

## TODO
- SSH authorization pending
