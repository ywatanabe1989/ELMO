<!-- ---
!-- title: 2024-12-22 15:54:46
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/Ninja/docs/dev_memo/separation_of_repositories.md
!-- --- -->

1. `ninja-core`
- Main package for Emacs
- Core elisp functionality
- `src/elisp/` and `src/prompts/`

2. `ninja-builders`
- Container and environment setup
- `src/apptainer_builders/`
- Installation scripts
- System configurations

3. `ninja-workspace`
- Template workspace
- Example projects
- Shared resources
- Documentation

Each repository should have its own:
- README.md
- LICENSE
- CI/CD workflows
- Version control
- Issue tracking

The `ninja-core` would be the main package, with others as dependencies.