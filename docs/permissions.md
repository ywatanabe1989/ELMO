<!-- ---
!-- title: ./Semacs/docs/permissions.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:33:20
!-- --- -->

755 breakdown:
- 7 (owner/host): full permissions (read=4, write=2, execute=1)
- 5 (semacs): read & execute only (read=4, execute=1)
- 5 (others): read & execute only

This is a good security practice where:
1. Host user maintains full control (7)
2. Semacs user can read/execute but not write (5)
3. Others have same restricted access (5)




1. Directory Structure:
```bash
$HOST_DOT_EMACS/
├── core/           # Core configurations (read-only for agents)
├── agents/         # Agent-specific configurations
│   └── ${AGENT_NAME}/
├── shared/         # Shared resources
└── server/         # Server socket directory
```

2. Modify create_semacs_user():
```bash
create_semacs_user() {
    # ... existing user creation code ...

    # Create structured directories
    mkdir -p "$SEMACS_DOT_EMACS/core"
    mkdir -p "$SEMACS_DOT_EMACS/agents"
    mkdir -p "$SEMACS_DOT_EMACS/shared"
    mkdir -p "$SEMACS_DOT_EMACS/server"

    # Set permissions
    chown -R "$SEMACS_USER:$SEMACS_GROUP" "$SEMACS_DOT_EMACS"
    chmod 755 "$SEMACS_DOT_EMACS"
    chmod 750 "$SEMACS_DOT_EMACS/core"
    chmod 770 "$SEMACS_DOT_EMACS/agents"
    chmod 770 "$SEMACS_DOT_EMACS/shared"
    chmod 770 "$SEMACS_DOT_EMACS/server"
}
```

This allows:
- Core configs protection
- Per-agent customization
- Shared resources access
- Controlled server socket access


