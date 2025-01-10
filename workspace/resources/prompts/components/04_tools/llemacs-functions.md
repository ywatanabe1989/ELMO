# Tool: llemacs-functions
```plaintext
llemacs: nil
llemacs--buf-disp: (buffer &optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-debug-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-debug-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-logging-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-logging-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-main-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-main-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-prompt-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-prompt-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-search-pj: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-disp-search-sys: (&optional file-path enable-q enable-readonly enable-org)
llemacs--buf-update-pj: nil
llemacs--check-json: (json-path)
llemacs--commit-and-push: (files message branch)
llemacs--create-ticket: (title body)
llemacs--def-buf-disp: (name sys-var pj-var)
llemacs--ensure-elisp-code: (code)
llemacs--ensure-gh-auth: nil
llemacs--extract-elisp-blocks: (text)
llemacs--extract-elisp-blocks-as-code: (text)
llemacs--format-log-entry: (log-entry)
llemacs--get-or-create-if-not-exists: (path)
llemacs--gh-create-issue: (bin title body)
llemacs--gh-create-pr: (bin title body base)
llemacs--git-add: (bin files)
llemacs--git-add-and-commit: (dir message)
llemacs--git-commit: (bin message)
llemacs--git-configure: nil
llemacs--git-ensure-branch: (bin branch)
llemacs--git-ensure-repo: (dir)
llemacs--git-init: (dir)
llemacs--git-log: (dir &optional limit)
llemacs--git-protect-main: (dir)
llemacs--git-push: (bin branch)
llemacs--git-reset: (dir &optional n-commits)
llemacs--git-resolve-conflicts: (dir)
llemacs--git-setup-gitignore: (dir)
llemacs--git-track: (dir files)
llemacs--git-unstage: (dir &optional files)
llemacs--git-untrack: (dir files)
llemacs--git-with-recipe-config: (dir recipe-name func &rest args)
llemacs--github-login: nil
llemacs--list-groups: (&optional pattern)
llemacs--llm-api-call-wrapper: (provider func &rest args)
llemacs--llm-api-check-rate-limit: (provider)
llemacs--llm-api-handle-error: (err provider)
llemacs--llm-claude: (text)
llemacs--llm-deepseek: (text)
llemacs--llm-gemini: (text)
llemacs--llm-prompt-compile: (recipe-id)
llemacs--llm-prompt-embed: (prompt recipe-id)
llemacs--llm-prompt-ensure-markdown-files: nil
llemacs--llm-prompt-get-available-recipe-ids: nil
llemacs--llm-prompt-get-recipe: (recipe-id)
llemacs--llm-prompt-get-template: (prompt-template-name)
llemacs--llm-prompt-get-templates: (&rest prompt-template-names)
llemacs--llm-prompt-open-templates: nil
llemacs--llm-prompt-write-compiled: (recipe-id content)
llemacs--llm-prompt2elisp: (prompt &optional recipe-id)
llemacs--llm-switch-provider: (provider)
llemacs--load-all-recipes: nil
llemacs--load-base-components: nil
llemacs--load-components: nil
llemacs--load-integration-components: nil
llemacs--load-json-file: (json-path)
llemacs--load-llm-components: nil
llemacs--load-logging-components: nil
llemacs--load-markdown-file: (file-path)
llemacs--load-project-management-components: nil
llemacs--load-recipe-file: (file)
llemacs--load-run-components: nil
llemacs--load-yaml-file: (file)
llemacs--logging-auto-refresh-mode: nil
llemacs--logging-backup-files: nil
llemacs--logging-define-loggers-sys: nil
llemacs--logging-ensure-log-file: (file-path)
llemacs--logging-format-message: (level message &optional full-project-name)
llemacs--logging-get-caller-info: nil
llemacs--logging-get-level-value: (level)
llemacs--logging-get-log-entries: (file-path)
llemacs--logging-get-logs: (&optional level is-pj)
llemacs--logging-get-logs-pj: (&optional level)
llemacs--logging-get-logs-sys: (&optional level)
llemacs--logging-meets-threshold-p: (level)
llemacs--logging-refresh: nil
llemacs--logging-refresh-buffer: nil
llemacs--logging-refresh-files: nil
llemacs--logging-should-log-p: (level)
llemacs--logging-view: (&optional select-level is-pj)
llemacs--logging-write: (level message &optional project-id enable-display)
llemacs--logging-write-api-pj: (message &optional project-id)
llemacs--logging-write-debug-pj: (message &optional project-id)
llemacs--logging-write-elisp-pj: (message &optional project-id)
llemacs--logging-write-error-pj: (string &rest args)
llemacs--logging-write-info-pj: (message &optional project-id)
llemacs--logging-write-prompt-pj: (message &optional project-id)
llemacs--logging-write-quiet: (content file-path &optional append)
llemacs--logging-write-search-pj: (message &optional project-id)
llemacs--logging-write-success-pj: (message &optional project-id)
llemacs--logging-write-warn-pj: (message &optional project-id)
llemacs--mermaid-compile: (&optional filename buffer)
llemacs--org-export-to-pdf: (org-file pdf-file)
llemacs--org-setup-visualization: (buf pdf-file)
llemacs--org-write-figure: (figure-path width)
llemacs--org-write-standard-headers: (title)
llemacs--path-create-log-paths-sys: nil
llemacs--path-define-log-paths-pj: nil
llemacs--path-find-bin: (name &rest alternatives)
llemacs--path-logs-all-pj: t
llemacs--path-logs-backup-pj: t
llemacs--path-logs-by-level-pj: t
llemacs--path-logs-pj: t
llemacs--path-logs-update-pj: nil
llemacs--path-pj-cat-config: (filename title)
llemacs--path-pj-get-or-create-data: (relative-file-path-from-the-data-dir)
llemacs--path-pj-get-or-create-figure: (filename title)
llemacs--path-pj-get-or-create-report-org: (title)
llemacs--path-pj-get-or-create-report-pdf: (title)
llemacs--path-pj-get-or-create-script-elisp: (filename)
llemacs--path-pj-get-or-create-script-python: (filename)
llemacs--path-pj-get-or-create-script-shell: (filename)
llemacs--path-pj-get-or-create-table: (filename title)
llemacs--path-pj-update: nil
llemacs--path-pj-update-config: (filename title)
llemacs--pj-auto-set: nil
llemacs--pj-buf-debug: t
llemacs--pj-buf-logging: t
llemacs--pj-buf-main: t
llemacs--pj-buf-prompt: t
llemacs--pj-buf-search: t
llemacs--pj-buf-update: nil
llemacs--pj-collect-context: (full-pj-name &optional log-level)
llemacs--pj-get-available-pjs: nil
llemacs--pj-get-contents: (path)
llemacs--pj-get-contents-log: (full-pj-name &optional log-level)
llemacs--pj-get-contents-pm-mmd: nil
llemacs--pj-get-contents-tree: (full-pj-name)
llemacs--pj-get-cur-pj: nil
llemacs--pj-get-default-pj: nil
llemacs--pj-get-dir: (full-pj-name)
llemacs--pj-get-last-pj: nil
llemacs--pj-get-latest-pj: nil
llemacs--pj-get-next-id: nil
llemacs--pj-get-path-log: (full-pj-name &optional log-level)
llemacs--pj-init: (pj-name &optional goals)
llemacs--pj-init-default: nil
llemacs--pj-init-pm-mmd: (full-pj-name pj-goals)
llemacs--pj-lock-acquire: (pj-id)
llemacs--pj-lock-check: (pj-id)
llemacs--pj-lock-check-stale: (pj-id)
llemacs--pj-lock-cleanup: nil
llemacs--pj-lock-cleanup-all: nil
llemacs--pj-lock-force-release: (pj-id &optional force)
llemacs--pj-lock-path: (pj-id)
llemacs--pj-lock-release: (pj-id)
llemacs--pj-mermaid-compile: nil
llemacs--pj-set-cur-pj: (pj-id &optional force)
llemacs--pj-set-last-pj: nil
llemacs--pj-switch: (pj-id &optional force)
llemacs--run-advance-project: nil
llemacs--run-elisp: (elisp-code)
llemacs--run-elisp-local: (elisp-code)
llemacs--run-elisp-script: (script &optional args)
llemacs--run-elisp-server: (elisp-code &optional emacs-server-file)
llemacs--run-gh-command: (bin &rest args)
llemacs--run-git-command: (bin &rest args)
llemacs--run-progn: (prompt)
llemacs--run-prompt: (prompt &optional recipe-id)
llemacs--run-prompt-after-run-hook-error: (llemacs--logging-write-error-pj prompt-text)
llemacs--run-prompt-after-run-hook-success: (elisp-code prompt-text)
llemacs--run-shell-command: (command)
llemacs--run-step: nil
llemacs--run-update-project-management: nil
llemacs--safe-read-file: (path)
llemacs--sanitize-content: (content)
llemacs--sanitize-filepath: (path)
llemacs--script-before-save-hook: nil
llemacs--script-detect-type: (file-path)
llemacs--script-format-header: (type file-path)
llemacs--script-get-pattern: (type)
llemacs--script-get-template: (type)
llemacs--script-update-header: (file-path)
llemacs--timestamp-get: nil
llemacs--timestamp-set: nil
llemacs--update-docs: nil
llemacs--validate-paths: (paths)
llemacs--validate-pj-id: (pj-id)
llemacs-list: (type &optional pattern)
llemacs-llm: (prompt &optional recipe-id)
llemacs-pj-step: nil
llemacs-pj-switch: (pj-id &optional force)
llemacs-pj-update-mermaid: nil
llemacs-run-auto: (&optional n)
llemacs-run-auto-async: (&optional n)
llemacs-run-steps: (&optional n-steps)
llemacs-run-steps-async: (&optional n-steps)
llemacs-timestamp: nil
my/tab-llemacs: nil
```
