# Tool: llemacs-variables
```plaintext
llemacs--logging-level-threshold
Minimum log level to record.
Value: "info"

llemacs--llm-api-key-anthropic
API key for Anthropic Claude.
Value: [MASKED]

llemacs--path-logs-api-pj
Log file paths for api for current project. (= ‘llemacs--path-pj-logs-api’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/api.log"

llemacs--path-python-env-sys
Path to the Python environment used by llemacs.el.
Value: "/home/ywatanabe/proj/llemacs/workspace/.env"

llemacs--orchestrator-options

Value: "((\"project-management\" . llemacs--run-project-management) (\"next-step\" . llemacs--run-next-step) (\"analyze\" . llemacs--run-analyze) (\"search\" . llemacs--run-search) (\"visualize\" . llemacs--run-visualize) (\"report\" . llemacs--run-report) (\"optimize\" . llemacs--run-optimize) (\"refactor\" . llemacs--run-refactor) (\"test\" . llemacs--run-test) (\"debug\" . llemacs--run-debug) (\"document\" . llemacs--run-document) (\"exit\" lambda nil (setq llemacs--orchestrator-running nil)))"

llemacs--path-res-scripts
Directory for LLEMACS scripts.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/scripts"

llemacs--run-n-steps
The number of steps for "llemacs--run-steps"
Value: "7"

llemacs--path-res-prompts
Directory for LLEMACS templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts"

llemacs--path-pj-pm-mmd
Project management directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/project_management/project_management.mmd"

llemacs--buf-prompt-sys
Buffer for prompt operations.
Value: "*LLEMACS-PROMPT*"

llemacs--path-logs-info-sys
Path to LLEMACS system Information level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/info.log"

llemacs--path-logs-api-sys
Path to LLEMACS system API interaction logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/api.log"

llemacs--path-logs-all-sys
File path for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/all"

llemacs--path-latest-project-id
File to store the latest project ID.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/.project-id"

llemacs--llm-engine-deepseek
Model for DeepSeek.
Value: "deepseek-chat"

llemacs--path-logs-search-sys
Path to LLEMACS system Search operation logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/search.log"

llemacs--logging-main-log-threshold
Threshold level for including logs in main.log file.
Value: "info"

llemacs--project-splitter
Splitter between logs.
Value: "
----------------------------------------
"

llemacs--path-logs-error-sys
Path to LLEMACS system Error level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/error.log"

llemacs--path-pj-logs-backup
Directory for current project logs backup.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/backup"

llemacs--path-pj-python
Python binary path for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/.env/bin/python"

llemacs--llm-api-key-groq
API key for Groq.
Value: [MASKED]

llemacs--llm-api-timeout
Default timeout for API requests in seconds.
Value: "30"

llemacs--logging-splitter
Splitter between logs.
Value: "
----------------------------------------
"

llemacs--log-levels-pj
Project-level logging-levels
Value: "((debug 0 . \"Debug level logs\") (info 1 . \"Information level logs\") (success 1 . \"Success level logs\") (prompt 1 . \"Prompt operation logs\") (elisp 1 . \"Elisp execution logs\") (api 1 . \"API interaction logs\") (search 1 . \"Search operation logs\") (warn 2 . \"Warning level logs\") (error 3 . \"Error level logs\"))"

llemacs--path-pj-config
Configuration directory for the current project configuration.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/config"

llemacs--path-pj-root
Root directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project"

llemacs--path-pj-logs-api
Log file paths for api for current project. (= ‘llemacs--path-logs-api-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/api.log"

llemacs--path-pj-logs-all
File for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/all.log"

llemacs--path-agents
Directory for LLEMACS agents.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents"

llemacs--path-res-prompt-compiled
Directory for compiled prompt templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/compiled"

llemacs--path-res
Directory for LLEMACS resources.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources"

llemacs--path-res-prompt-components
Directory for prompt template components.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/components"

llemacs--path-agent
User-specific home directory for LLEMACS.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents/ywatanabe"

llemacs--path-res-secrets
Directory for storing sensitive information.
Value: [MASKED]

llemacs-project-auto-save
Whether to automatically save project state.
Value: "t"

llemacs--path-logs-prompt-sys
Path to LLEMACS system Prompt operation logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/prompt.log"

llemacs--llm-engine-groq
Model for Groq.
Value: "llama-3.3-70b-versatile"

llemacs--logging-enable-display-threshold
Threshold level for displaying log messages. Logs at this level and above will be displayed.
Value: "error"

llemacs--llm-api-call-timestamps
Hash table tracking API call timestamps per provider.
Value: "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ())"

llemacs-git-auto-add
Whether to automatically stage modified files.
Value: "t"

llemacs--path-pj-logs-info
Log file paths for info for current project. (= ‘llemacs--path-logs-info-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/info.log"

llemacs--path-logs-search-pj
Log file paths for search for current project. (= ‘llemacs--path-pj-logs-search’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/search.log"

llemacs--path-pj-logs
Directory for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs"

llemacs--path-pj-lock
Lock file for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/.lock"

llemacs--script-supported-types
List of supported script types.
Value: "(css py el sh yaml org md tex)"

llemacs--path-workspace
Base directory for LLEMACS workspace.
Value: "/home/ywatanabe/proj/llemacs/workspace"

llemacs--path-pj-logs-prompt
Log file paths for prompt for current project. (= ‘llemacs--path-logs-prompt-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/prompt.log"

llemacs--llm-engine-anthropic
Model for Anthropic Claude.
Value: "claude-3-5-sonnet-20241022"

llemacs--path-logs-success-sys
Path to LLEMACS system Success level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/success.log"

llemacs--path-pj-logs-search
Log file paths for search for current project. (= ‘llemacs--path-logs-search-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/search.log"

llemacs--buf-search-pj
Buffer for search results.
Value: "*LLEMACS-SEARCH (105-epilepsy-prediction-project)*"

llemacs--path-pj-win-shortcut
Path for Windows symbolic link creation.
Value: "C:\\Users\\wyusu\\Documents\\current-project-win"

llemacs--cur-pj
Currently active project ID in the form of <id>-<project-name>.
Value: "105-epilepsy-prediction-project"

llemacs--path-logs-info-pj
Log file paths for info for current project. (= ‘llemacs--path-pj-logs-info’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/info.log"

llemacs--buf-debug-pj
Buffer for debug output when debug mode enabled.
Value: "*LLEMACS-DEBUG (105-epilepsy-prediction-project)*"

llemacs--buf-search-sys
Buffer for search results.
Value: "*LLEMACS-SEARCH*"

llemacs--llm-prompt-available-recipe-ids
List of available prompt-recipe IDs.
Value: "nil"

llemacs--path-logs-warn-sys
Path to LLEMACS system Warning level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/warn.log"

llemacs--path-logs-elisp-sys
Path to LLEMACS system Elisp execution logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/elisp.log"

llemacs--llm-api-endpoints
API endpoints for different LLM providers.
Value: "((openai . \"https://api.openai.com/v1\") (anthropic . \"https://api.anthropic.com\") (cohere . \"https://api.cohere.ai/v1\"))"

llemacs--path-res-templates
Directory for LLEMACS templates.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/templates"

llemacs--llm-api-key-deepseek
API key for DeepSeek.
Value: [MASKED]

llemacs--path-agent-emacs-server
Path to LLEMACS Emacs server socket.
Value: "/home/ywatanabe/proj/llemacs/workspace/agents/ywatanabe/.emacs.d/emacs-server/server"

llemacs--timestamp
Timestamp references by Llemacs.
Value: "2025-0111-042843"

llemacs--path-pj-logs-main
File for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/104-epilepsy-prediction-project/logs/main.log"

llemacs--path-logs-prompt-pj
Log file paths for prompt for current project. (= ‘llemacs--path-pj-logs-prompt’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/prompt.log"

llemacs--path-pj-scripts
Scripts directory for the current project scripts.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/scripts"

llemacs--project-status-options
Available status options for LLEMACS projects.
Value: "(\"planning\" \"in-progress\" \"review\" \"completed\" \"archived\")"

llemacs--tab-counter
Counter for LLEMACS tab numbering.
Value: "0"

llemacs--logging-max-size
Maximum size of log files in bytes (10MB default).
Value: "10485760"

llemacs--orchestrator-running
Flag to control orchestrator loop.
Value: "t"

llemacs--path-pj-data
Data directory for the current project data.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/data"

llemacs--path-python-sys
Path to the Python binary used by llemacs.el.
Value: "/home/ywatanabe/proj/llemacs/workspace/.env/bin/python"

llemacs--path-pj-logs-elisp
Log file paths for elisp for current project. (= ‘llemacs--path-logs-elisp-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/elisp.log"

llemacs--llm-engine-google
Model for Google Claude.
Value: "gemini-2.0-flash-exp"

llemacs--path-logs-debug-pj
Log file paths for debug for current project. (= ‘llemacs--path-pj-logs-debug’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/debug.log"

llemacs--buf-main-sys
Name of main buffer.
Value: "*LLEMACS-MAIN*"

llemacs--path-logs-sys
Directory for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs"

llemacs--path-pj-results-figures
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/results/figures"

llemacs--logging-refresh-interval
Interval in seconds for auto-refreshing log buffer.
Value: "5"

llemacs--path-logs-error-pj
Log file paths for error for current project. (= ‘llemacs--path-pj-logs-error’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/error.log"

llemacs--path-pj-results-tables
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/results/tables"

llemacs--buf-debug-sys
Buffer for debug output when debug mode enabled.
Value: "*LLEMACS-DEBUG*"

llemacs--path-pj
Root directory for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project"

llemacs--llm-provider
Switcher for LLM provider
Value: "google"

llemacs--llm-api-min-delay
Minimum delay between API calls in seconds.
Value: "1.0"

llemacs--llm-prompt-recipes
List of prompt recipe definitions loaded from recipe files.
Value: "nil"

llemacs--buf-logging-pj
Name of log buffer.
Value: "*LLEMACS-LOGGING (105-epilepsy-prediction-project)*"

llemacs--path-logs-warn-pj
Log file paths for warn for current project. (= ‘llemacs--path-pj-logs-warn’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/warn.log"

llemacs--path-res-prompt-recipes
Directory for prompt template recipes.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/prompts/recipes"

llemacs--llm-gemini-script
Python script for calling Gemini
Value: "/home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-llm/03-core-gemini_call.py"

llemacs--path-pj-logs-warn
Log file paths for warn for current project. (= ‘llemacs--path-logs-warn-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/warn.log"

llemacs--path-logs-debug-sys
Path to LLEMACS system Debug level logs file.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level/debug.log"

llemacs--buf-prompt-pj
Buffer for prompt operations.
Value: "*LLEMACS-PROMPT (105-epilepsy-prediction-project)*"

llemacs--git-auto-commit
Whether to automatically commit changes.
Value: "t"

llemacs--path-pj-logs-by-level
Directory for current project logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level"

llemacs--path-pj-logs-error
Log file paths for error for current project. (= ‘llemacs--path-logs-error-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/error.log"

llemacs--script-header-templates
Templates for file headers.
Value: "((css . \"/* Timestamp: \\\"%s (%s)\\\" */
/* File: %s */

\") (py . \"#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Timestamp: \\\"%s (%s)\\\"
# File: %s

__file__ = \\\"%s\\\"

\") (el . \";;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: %s
;;; Timestamp: <%s>
;;; File: %s
\") (sh . \"#!/bin/bash
# Timestamp: \\\"%s (%s)\\\"
# File: %s

THIS_DIR=\\\"$(cd \\\"$(dirname \\\"${BASH_SOURCE[0]}\\\")\\\" && pwd)\\\"

\") (yaml . \"# Timestamp: \\\"%s (%s)\\\"
# File: %s
\") (org . \"# Timestamp: \\\"%s (%s)\\\"
# File: %s

\") (md . \"<!-- ---
!-- Timestamp: %s
!-- Author: %s
!-- File: %s
!-- --- -->
\") (tex . \"%% Timestamp: \\\"%s (%s)\\\"
%% File: %s

\"))"

llemacs--path
Base directory for LLEMACS project.
Value: "/home/ywatanabe/proj/llemacs/"

llemacs--git-gitignore-path
Path to global gitignore template file.
Value: "~/.git-templates"

llemacs--path-projects
Directory for LLEMACS projects.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects"

llemacs--path-logs-elisp-pj
Log file paths for elisp for current project. (= ‘llemacs--path-pj-logs-elisp’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/elisp.log"

llemacs--path-logs-by-level-sys
Directory for LLEMACS system logs.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/by_level"

llemacs--run-start-tag
Starting tag for Llemacs run
Value: "
================================================================================
Llemacs-run called.
================================================================================"

llemacs--path-res-tools
Directory for LLEMACS tools.
Value: "/home/ywatanabe/proj/llemacs/workspace/resources/tools"

llemacs--path-pj-reports
Reports directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/reports"

llemacs--path-pj-results
Resutls directory for the current project results.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/results"

llemacs--git-enabled
Whether to enable Git integration.
Value: "t"

llemacs--path-sample-project-zip
Sample project zip file for initializing project structure.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/000-sample-project.zip"

llemacs--buf-logging-sys
Name of log buffer.
Value: "*LLEMACS-LOGGING*"

llemacs--path-pj-logs-debug
Log file paths for debug for current project. (= ‘llemacs--path-logs-debug-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/debug.log"

llemacs--log-entry-limit
Maximum number of log entries to include in the context.
Value: "7"

llemacs--llm-api-key-google
API key for Google Claude.
Value: [MASKED]

llemacs-project-max-history
Maximum number of project history entries to keep.
Value: "50"

llemacs--path-logs-backup-sys
Directory for LLEMACS system logs backup.
Value: "/home/ywatanabe/proj/llemacs/workspace/logs/backup"

llemacs-project-default-name
Default project name when none specified.
Value: "default"

llemacs--path-logs-success-pj
Log file paths for success for current project. (= ‘llemacs--path-pj-logs-success’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/success.log"

llemacs--llm-api-retries
Number of retries for failed API requests.
Value: "3"

llemacs--path-pj-python-env
Python environment path for the current project.
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/.env"

llemacs--script-header-patterns
Regular expression patterns to match file headers.
Value: "((css . \"\\\\`\\\\(?:/\\\\* Timestamp:.*\\\\*/
\\\\)?\\\\(?:/\\\\* File:.*\\\\*/
\\\\)?\") (py . \"\\\\`\\\\(?:#!.*
\\\\)?\\\\(?:# -\\\\*-.*-\\\\*-
\\\\)?\\\\(?:# Timestamp:.*
\\\\)?\\\\(?:# File:.*
\\\\)?\\\\(?:
\\\\)?\\\\(?:__file__.*
\\\\)?
?\") (el . \"\\\\`\\\\(?:;;;.*
\\\\)\\\\{1,4\\\\}\") (sh . \"\\\\`\\\\(?:#!.*
\\\\)?\\\\(?:# Timestamp:.*
\\\\)?\\\\(?:# File:.*
\\\\)?\\\\(?:
\\\\)?\\\\(?:THIS_DIR=.*
\\\\)?
?\") (yaml . \"\\\\`\\\\(?:# Timestamp:.*
\\\\)?\\\\(?:# File:.*
\\\\)?\") (org . \"\\\\`\\\\(?:# Timestamp:.*
\\\\)?\\\\(?:# File:.*
\\\\)?\") (md . \"\\\\`\\\\(?:<!-- ---
\\\\)\\\\(?:!-- Timestamp:.*
\\\\)\\\\(?:!-- Author:.*
\\\\)\\\\(?:!-- File:.*
\\\\)\\\\(?:!-- --- -->
\\\\)\") (tex . \"\\\\`\\\\(?:%% Timestamp:.*
\\\\)?\\\\(?:%% File:.*
\\\\)?\"))"

llemacs--log-levels-sys
System-level logging-levels
Value: "((debug 0 . \"Debug level logs\") (info 1 . \"Information level logs\") (success 1 . \"Success level logs\") (prompt 1 . \"Prompt operation logs\") (elisp 1 . \"Elisp execution logs\") (api 1 . \"API interaction logs\") (search 1 . \"Search operation logs\") (warn 2 . \"Warning level logs\") (error 3 . \"Error level logs\"))"

llemacs--path-pj-logs-success
Log file paths for success for current project. (= ‘llemacs--path-logs-success-pj’)
See "llemacs--path-logs-update-pj".
Value: "/home/ywatanabe/proj/llemacs/workspace/projects/105-epilepsy-prediction-project/logs/by_level/success.log"

llemacs--timestamp-format
Timestamp references by Llemacs.
Value: "%Y-%m%d-%H%M%S"

llemacs--buf-main-pj
Name of main buffer.
Value: "*LLEMACS-MAIN (105-epilepsy-prediction-project)*"

```
