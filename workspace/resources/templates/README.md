<!-- ---
!-- title: 2024-12-23 23:27:25
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.emacs.d/lisp/Llemacs/workspace/resources/templates/README.md
!-- --- -->

---



---


---

## **Conclusion**

These templates and examples provide a solid foundation for your multi-agent system's workspace. By embedding tags within your `.md` files, you create a robust system for organizing and querying your files directly within Emacs. This approach enhances maintainability, scalability, and fosters a self-evolving environment.

Feel free to customize and expand upon these templates to suit the specific needs of your projects and agents.



Yes, using JSON files more extensively could offer some advantages, but your current approach with Markdown files is well-suited for LLM workflows and human readability. Here’s a breakdown of why your current structure is good and where JSON could fit better:

**Markdown Strengths (Your Current Approach):**

1.  **Human Readability:** Markdown is easy to read and write, which is ideal for documentation, notes, and agent memory. This facilitates human understanding and interaction.
2.  **LLM Friendliness:** LLMs can easily process and generate Markdown content, making it a good choice for prompts, reports, and text-based outputs.
3.  **Flexibility:** Markdown allows for a balance of structured and free-form text, which is suitable for notes and logs that don't always follow strict formats.
4.  **Version Control:** Markdown files are text-based, so they work well with version control systems.
5.  **Emacs Integration:** Emacs is excellent at editing and navigating Markdown files, making it suitable for your workflow.
6.  **Clear Separation of Concerns:**
    *   `README.md` files for directory context.
    *   Markdown for human-readable content.
    *   Tags for metadata.

**Potential Uses for JSON (Where It Can Improve):**

1.  **Structured Data:**
    *   **Agent Profiles:**  `llemacss/llemacs-{id}/profile.json` is excellent for structured data. You could expand this.
    *   **Tool Definitions:**  `shared/tools/tool-{id}.json` could hold structured information about tools, including required inputs, outputs, and elisp code snippets, and maybe add a JSON schema for validation.
    *   **Knowledge Base:** `shared/knowledge_base/knowledge.json` is a good place for structured information for agents.
    *   **Prompts**: `shared/prompts/prompt-{id}.json` can hold structured information about the prompt.
    *    **Task Management**: Task lists for llemacss in `llemacss/llemacs-{id}/tasks.json`

2.  **Inter-Agent Communication:** Standardize message structure by using JSON format for inter-agent communication.

3.  **Querying:** JSON can be easier to parse and query programmatically.

4.  **Consistent Data Structures:** Helps maintain a consistent data structure between agents.

**Why Your Markdown Focus Is Still Good:**

1.  **LLM Workflow:** LLMs are good at parsing and outputting natural language. Markdown is more conducive to natural language than JSON.
2.  **Human-in-the-Loop:** Markdown’s readability is important because there will be humans interpreting the information.

**Recommendations:**

1.  **Use JSON for Structured Data and Inter-Agent Communication:**
    *   Keep `profile.json` files.
    *   Add `tool.json` files to `shared/tools/tool-{id}` directory.
    *   Add `knowledge.json` for agent knowledge base.
    *   Add `prompts/prompt-{id}.json` for prompts.
    *   Use JSON for all message communication between agents `messages/inbox/*.json` and `messages/outbox/*.json`.
    *   Add `tasks.json` for tasks to agents memory
2.  **Keep Markdown for Everything Else:**
    *   Keep Markdown for all documentation, notes, memory, reports, logs, and so on.
    *   Continue to use tags in your markdown files
3. **Combine Markdown with JSON in Memory Files:**
    * Consider using a structured JSON section in `memory/*.md` for more complex structured information and keeping the readable markdown part, this may improve LLM's parsing, or use JSON files with linked markdown files.

**Example JSON `tool.json`:**

```json
{
    "tool_id": "tool-001",
    "tool_name": "Example Tool",
    "description": "This is an example tool.",
    "elisp_command": "(progn (command1 arg1 arg2))",
    "input": {
      "type": "object",
      "properties": {
          "arg1": { "type": "string", "description": "Argument 1" },
          "arg2": { "type": "integer", "description": "Argument 2" }
        },
      "required": ["arg1","arg2"]
    },
    "output": {
        "type": "object",
        "properties": {
            "result": {"type": "string", "description": "The result of the tool"},
          "status": {"type": "string", "description": "Status of the tool's execution"}
        },
        "required": ["result", "status"]
    }
}
```

**Example `messages/inbox/YYYY-MM-DD-{subject}-from-<from>-to-<to>.json`**

```json
{
  "message_id": "unique-id",
  "timestamp": "YYYY-MM-DD-HH-MM-SS",
  "subject": "Data Issues Found",
  "from": "Alex Smith",
  "to": "Jordan Lee",
  "cc": [],
  "message": "Hi Jordan,\n\nI noticed that there are several missing values in the `purchase_amount` column of the dataset. We need to decide on an appropriate method to handle these missing entries. Do you have any suggestions?\n\nBest,\nAlex",
   "attachments": ["/path/to/attachment1", "/path/to/attachment2"],
   "references": ["/path/to/document1"]
}
```

**Example of `memory/YYYY-MM-DD-<title>.md`**

```markdown
---
title: "Initial Data Exploration"
available tags: [summary, success, failure, tools utilized, data referenced, ideas, next steps]
date: 2023-10-05
---

# Initial Data Exploration

## **Summary**

Today, I began exploring the customer transaction dataset for Project 001.

```json
{
  "successes": [
    "Successfully loaded and previewed the dataset.",
    "Identified key variables for analysis."
  ],
  "failures": [
    "Encountered issues with missing values in the `purchase_amount` column."
  ],
   "tools_utilized": [
    {
        "tool_id": "tool-001",
        "tool_name": "Pandas",
        "description": "Tool to process data"
    }
    ],
  "data_referenced": [
    "/path/to/dataset"
  ],
  "ideas": [
      "Implement a data imputation strategy for missing values.",
      "Consider feature engineering on the `purchase_date`."
    ],
  "next_steps": [
    "Address missing values in the dataset.",
    "Start preliminary data analysis and visualization."
  ]
}
```

## **Details**
...

## **Next Steps**
...
```

**Conclusion:**

Your approach with Markdown is excellent for human readability, LLM processing, and is compatible with Emacs. By adding JSON for agent profiles, tool definitions, knowledge base and task management, messages and structured data, you can create a more robust and versatile system, and by mixing JSON sections with Markdown files in memory, you can keep human readability while improving LLM's parsing ability, which is useful if you aim to create a self-evolving system with LLMs.



workspace/
├── shared/
│   ├── TAGS.md
│   ├── agents/
│   │   ├── README.md
│   │   ├── templates/
│   │   │   └── agent-template.md (tags: role, available tools, ...)
│   │   └── configs/
│   │       └── agent-config.json
│   ├── tools/
│   │   ├── README.md
│   │   ├── tool-{id}/
│   │   |   ├── tool.md (tags: tool-name, usage via elisp)
│   │   |   └── tool.json (tags: tool-name, description, elisp command, input schema, output schema)
│   │   └── tool-schema.json (tags: schema, validation)
│   ├── prompts/
│   │   ├── README.md
│   │   ├── prompt-template.md (tags: why, what, which, where, when, how, expected output, output format)
│   │   └── prompt-{id}.json (tags: prompt description, input schema, output schema)
│   ├── knowledge_base/
|   |   ├── README.md
|   |   └── knowledge.json (tags: knowledge, schema)
│   └── system/
│       ├── README.md
│       ├── logs/
│       │   └── system.log (tags: date, agent-id, error level (info, warnings, error, note))
├── projects/
│   ├── README.md (tags: projects overview)
│   └── project-{id}/
│       ├── README.md (tags: aim, assignments)
│       ├── .env
│       ├── scripts/
│       │   └── README.md
│       ├── data/
│       │   ├── README.md (tags: data_structure)
│       │   └── dataset.{ext}
│       ├── docs/
│       │   ├── README.md (tags: task, status, issues)
│       ├── outputs/
│       │   ├── README.md (tags: reports, results)
│       │   └── report-{id}/
│       │       ├── README.md (tags: title, what's new, achieved)
│       │       ├── data/ (symlink)
│       │       └── report.pdf (tags: overview, methods, figures, tables)
│       ├── issues/
│       │   └── YYYY-MM-DD-<title>-<issue-id>.md (tags: )
└── llemacss/
    ├── README.md
    └── llemacs-{id}/
        ├── profile.json (tags: llemacs name in project, role)
        ├── status.md (tags: todo, waiting, pending, done, canceled, reason)
        ├── memory/
        │   ├── README.md
        │   └── YYYY-MM-DD-<title>.md (tags: summary, success, failure, tool, data, idea)
        │   |   └── YYYY-MM-DD-<title>.json (tags:  summary, success, failure, tools utilized, data referenced, ideas, next steps)
        ├── messages/
        │   ├── README.md
        │   ├── inbox/
        │   │    └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.json (tags: subject, from, to, cc, content, attachment, reference...)
        │   ├── outbox/
        │   │    └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.json (tags: subject, from, to, cc, content, attachment, reference...)
        │   └── README.md
        ├── tasks.json (tags: task list, status)
        └── projects/
            ├── README.md
            └── project-{id}/
                ├── README.md (tags: project title, project description, goal, background, methods, results, outputs, issues, progress, todo, pending, waiting, in-progress, done, canceled)
                ├── forum.md
                ├── messages/
                │   ├── README.md
                │   ├── inbox/
                │   │   ├── README.md
                │   │   └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.json (tags: subject, from, to, cc, content, attachment, reference...)
                │   └── outbox/
                │       ├── README.md
                │       └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.json (tags: subject, from, to, cc, content, attachment, reference...)