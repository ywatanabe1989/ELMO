<!-- ---
!-- Timestamp: 2025-01-11 18:13:59
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/docs/installation/git.md
!-- --- -->

# Setting Up Git Environment Variables

You can optionally configure git for the system by exporting variables, for example:

```bash
export LLEMACS_HOST_HOME="$HOME/proj/llemacs"
export LLEMACS_GIT_EMAIL="agent@llemacs.com"
export LLEMACS_GIT_USER_NAME="agent@llemacs.com"
export LLEMACS_GIT_GITIGNORE_PATH="$LLEMACS_HOST_HOME/.gitignore"
```

These are then used by Llemacs to automate commits, pushes, etc., when that feature is enabled.
<!-- # Setup Git Environmental variables
 !-- ``` bash
 !-- export LLEMACS_HOST_HOME="$HOME/proj/llemacs"
 !-- export LLEMACS_GIT_EMAIL="agent@llemacs.com"
 !-- export LLEMACS_GIT_USER_NAME="agent@llemacs.com"
 !-- export LLEMACS_GIT_GITIGNORE_PATH="$LLEMACS_HOST_HOME/.gitignore"
 !-- ``` -->
