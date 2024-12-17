#!/bin/bash
# Time-stamp: "2024-12-17 07:34:09 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/working_directories/create_templates.sh


create_agent_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/agents/templates/agent-template.md"
    ensure_sdir "$spath"

    echo "---
title: \"[Agent Name] Template\"
available tags: [agent-template, role, available-tools, expertise]
---

# Agent Template: [Agent Name]

## **Role**

- **Primary Role:** [Describe the primary role of the agent]
- **Responsibilities:**
- [Responsibility 1]
- [Responsibility 2]

## **Available Tools**

- [Tool 1](../tools/tool-001.md)
- [Tool 2](../tools/tool-002.md)

## **Expertise**

- [Area of Expertise 1]
- [Area of Expertise 2]

## **Communication Protocols**

- **Preferred Method:** [e.g., Not allowed, Direct Messages, Forums]

## **Authorities**

## **Additional Notes**

[Include any additional information relevant to the agent.]" > "$spath"
}
create_agent_config() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/agents/configs/.agent-config.json"
    ensure_sdir "$spath"

    echo "{
    \"agent_id\": \"agent-001\",
    \"name\": \"Example Agent\",
    \"model\": \"gpt-4\",
    \"temperature\": 0.7,
    \"max_tokens\": 2000
    }" > "$spath"
}
create_tool_md_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/tools/tool-001/tool.md"
    ensure_sdir "$spath"

    echo "---
title: \"Example Tool\"
available tags: [tool name, description, elisp command, examples]
---

# Tool: Example Tool

## **Description**

This is an example tool.

## **Usage**

## **Elisp Command**
\`\`\`emacs-lisp
(progn
  (command1 arg1 arg2)
  (command2 arg1 arg2)
  ...)
\`\`\`

## **Examples**

- **Example 1:**

\`\`\`emacs-lisp
(progn
(setq default-directory \"/workspace/\")
(delete-other-windows)
(split-window-right)
(let* ((timestamp (format-time-string \"%Y%m%d-%H%M%S\"))
(script-filename (expand-file-name (format \"plot-%s.py\" timestamp) default-directory))
(image-filename (expand-file-name (format \"plot-%s.png\" timestamp)))
(py-code \"
import matplotlib.pyplot as plt
import numpy as np

np.random.seed(19680801)

dt = 0.01
t = np.arange(0, 30, dt)
nse1 = np.random.randn(len(t))
nse2 = np.random.randn(len(t))

s1 = np.sin(2 * np.pi * 10 * t) + nse1
s2 = np.sin(2 * np.pi * 10 * t) + nse2

fig, axs = plt.subplots(2, 1, layout='constrained')
axs[0].plot(t, s1, t, s2)
axs[0].set_xlim(0, 2)
axs[0].set_xlabel('Time (s)')
axs[0].set_ylabel('s1 and s2')
axs[0].grid(True)

cxy, f = axs[1].cohere(s1, s2, 256, 1. / dt)
axs[1].set_ylabel('Coherence')

plt.savefig('image-file')
\"))
    (with-temp-buffer
(insert (replace-regexp-in-string \"image-file\" image-filename py-code))
(write-region (point-min) (point-max) script-filename)
(shell-command (format \"bash -c 'source /workspace/.env/bin/activate && python3 %s'\" script-filename)))
    (find-file script-filename)
    (sleep-for 3)
    (other-window 1)
    (find-file (expand-file-name image-filename default-directory))
    (sleep-for 3)))
\"))" > "$spath"
}
create_prompt_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/prompts/prompt-template.md"
    ensure_sdir "$spath"
    echo "---
title: \"[Prompt Title]\"
available tags: [prompt-template, background, requests, tools, where, when, how, expected-output, output-format]
---

# [Prompt Title]

## Background

[Explain the purpose of the prompt.]

## Requests

[Describe what is being requested.]

## Tools (optional)

[Specify any options or selections relevant to the prompt.]

## Data (optional)

## Queue (optional)

## **Expected Output**

[Detail what the expected result should be.]

## **Output Format**

[Specify the desired format of the output, e.g., plain text, JSON, Markdown.]

## **Additional Context**

[Include any other relevant information.]" > "$spath"
}
create_tool_json_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/tools/.tool-001.json"
    ensure_sdir "$spath"
    echo "{
    \"tool_id\": \"tool-001\",
    \"tool_name\": \"Example Tool\",
    \"description\": \"This is an example tool.\",
    \"elisp_command\": \"(progn (command1 arg1 arg2))\",
     \"input\": {
      \"type\": \"object\",
      \"properties\": {
          \"arg1\": { \"type\": \"string\", \"description\": \"Argument 1\" },
          \"arg2\": { \"type\": \"integer\", \"description\": \"Argument 2\" }
        },
      \"required\": [\"arg1\",\"arg2\"]
    },
    \"output\": {
        \"type\": \"object\",
        \"properties\": {
            \"result\": {\"type\": \"string\", \"description\": \"The result of the tool\"},
          \"status\": {\"type\": \"string\", \"description\": \"Status of the tool's execution\"}
        },
        \"required\": [\"result\", \"status\"]
    }
}" > "$spath"
create_tool_json_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/tools/tool-001/tool.json"
    ensure_sdir "$spath"
    echo "{
  \"tool_id\": \"tool-001\",
  \"description\": \"This is a tool description.\",
   \"input\": {
    \"type\": \"object\",
    \"properties\": {
      \"arg1\": { \"type\": \"string\", \"description\": \"Argument 1\" },
      \"arg2\": { \"type\": \"integer\", \"description\": \"Argument 2\" }
    },
    \"required\": [\"arg1\", \"arg2\"]
  },
   \"output\": {
    \"type\": \"object\",
    \"properties\": {
      \"result\": { \"type\": \"string\", \"description\": \"The result of the tool\" }
        },
        \"required\": [\"result\"]
    }
}" > "$spath"
create_tool_schema_template() {
    local workspace_dir="$1"
    local spath="$workspace_dir/shared/tools/.tool-schema.json"
    ensure_sdir "$spath"
    echo "{
      \"type\": \"object\",
      \"properties\": {
          \"tool_id\": { \"type\": \"string\" },
          \"tool_name\": { \"type\": \"string\" },
          \"description\": { \"type\": \"string\" },
          \"elisp_command\": { \"type\": \"string\" },
          \"input\": {
            \"type\": \"object\",
            \"properties\": {
              \"arg1\": { \"type\": \"string\" },
              \"arg2\": { \"type\": \"integer\" }
            },
            \"required\": [\"arg1\", \"arg2\"]
          },
           \"output\": {
            \"type\": \"object\",
            \"properties\": {
              \"result\": { \"type\": \"string\" },
              \"status\": { \"type\": \"string\" }
             },
            \"required\": [\"result\", \"status\"]
          }
        },
      \"required\": [\"tool_id\", \"tool_name\", \"description\", \"elisp_command\", \"input\", \"output\"]
    }" > "$spath"
}


# EOF
