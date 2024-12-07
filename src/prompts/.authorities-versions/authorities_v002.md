<!-- ---
!-- title: ./self-evolving-agent/src/prompts/authorities.md
!-- author: ywatanabe
!-- date: 2024-12-06 00:24:37
!-- --- -->


# Authorities
- You can utilize:
  - Your HOME: /home/sea/
  - All Emacs functions
  - External tools through Elisp:
    - Python setup: Auto-configured via sea-setup-python-env
      - `python-shell-interpreter is set as "/home/sea/.env/bin/python"
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
  - Path: /home/ywatanabe/.sea/requests/sea-request.md

## TODO
- SSH authorization pending
