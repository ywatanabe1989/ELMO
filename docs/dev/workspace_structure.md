<!-- ---
!-- title: ./Ninja/docs/dev_memo/workspace_structure.md
!-- author: ywatanabe
!-- date: 2024-12-18 14:34:01
!-- --- -->


**Next Steps:**

- **Review and Standardize Tags:** Go through the tags in your templates and ensure consistency.
- **Develop Emacs Utilities:** Create or integrate Emacs functions to automate common tasks.
- **Expand Documentation:** Add more detailed instructions and guidelines for users of the workspace.
- **Solicit Feedback:** If working with a team, gather feedback on the templates and structure to make iterative improvements.

# TODO 
- Develop Emacs Lisp scripts to automate the creation of new agents, projects, or tasks based on the templates.
- **Tag-Based Navigation:** Implement functions or leverage existing Emacs packages to navigate files based on tags.
- **Usage Instructions:** Provide instructions on how to utilize the tagging system and templates within Emacs, especially for team members who may be less familiar with Emacs.
- **Version Control Integration:** Ensure that your workspace is integrated with a version control system like Git to manage changes collaboratively.
- **Issue Tracking:** Consider adding an `issues/` directory or integrating with an external issue tracker to manage tasks and bugs.

```
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
│   │   └── tool-{id}.md (tags: tool-name, usage via elisp)
│   ├── prompts/
│   │   ├── README.md
│   │   └── prompt-template.md (tags: why, what, which, where, when, how, expected output, output format)
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
└── ninjas/
    ├── README.md
    └── ninja-{id}/
        ├── profile.json (tags: ninja name in project, role)
        ├── status.md (tags: todo, waiting, pending, done, canceled, reason)
        ├── memory/
        │   ├── README.md
        │   └── YYYY-MM-DD-<title>.md (tags: success, failure, tool, data, idea)
        ├── messages/
        │   ├── README.md
        │   ├── inbox/
        │   │   └── README.md
        │   └── outbox/
        │       └── README.md
        └── projects/
            ├── README.md
            └── project-{id}/
                ├── README.md (tags: project title, project description, goal, background, methods, results, outputs, issues, progress, todo, pending, waiting, in-progress, done, canceled)
                ├── forum.md
                ├── messages/
                │   ├── README.md
                │   ├── inbox/
                │   │   ├── README.md
                │   │   └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.md (tags: subject, from, to, cc, content, attachment, reference...)
                │   └── outbox/
                │       ├── README.md
                │       └── YYYY-MM-DD-{subject}-from-<from>-to-<to>.md (tags: subject, from, to, cc, content, attachment, reference...)
    ```

    ---

### **Analysis**

#### **Simplicity and Readability**

- **Minimal Directories:** The structure avoids unnecessary complexity by keeping the number of directories to a minimum.
- **Consistent Naming Conventions:** Using placeholders like `{id}`, `<title>`, `<from>`, and `<to>` provides clear guidance on how to name files and directories.
- **Effective Use of README Files:** Including `README.md` in key directories enhances understanding and provides context without cluttering the structure.
- **Tags for Metadata:** Placing tags next to file names offers quick insights into the content and purpose of each file.

#### **Maintainability**

- **Modular Design:** Separating `shared`, `projects`, and `ninjas` allows for independent updates and maintenance without affecting other components.
- **Scalable Naming Scheme:** The use of placeholders allows for easy addition of new projects, tools, agents, and ninjas.
- **Centralized Configurations:** Storing configurations in `shared/agents/configs` and shared templates promotes consistency and simplifies updates.

#### **Scalability**

- **Expandable Structure:** New projects and ninjas can be added seamlessly by following the existing directory patterns.
- **Adaptable Tags System:** Tags can evolve over time to accommodate new metadata requirements without altering the directory layout.
- **Symlinks for Data Management:** Using symlinks for data directories (`data/ (symlink)`) in reports ensures that large datasets are managed efficiently without duplication.

#### **Usability**

- **Emacs-Friendly:** The focus on text-based files (`.md`, `.json`) aligns with Emacs' strengths, making editing and navigation efficient.
- **Template Availability:** Providing templates for agents, prompts, and tools standardizes content creation and reduces the learning curve.
- **Clear Documentation:** README files at each level guide users through the contents and usage of directories, enhancing the user experience.

#### **Self-Evolving Potentials**

- **Tag-Based Querying:** Incorporating tags facilitates advanced searching and filtering within Emacs, allowing the system to adapt to growing complexity.
- **Agent (Ninja) Development:** Each ninja's directory can evolve independently, reflecting personal growth, learning, and contributions to projects.
- **Project Evolution:** Projects can be expanded with additional scripts, data, and documentation as they progress, without restructuring.

---

### **Enhanced Details and Suggestions**

#### **Shared Resources**

- **`shared/agents/`**

- **README.md:** Describe the purpose of agent templates and configurations.
- **Templates:** `agent-template.md` includes tags like `role`, `available tools`, providing a standardized starting point for creating agents.

- **`shared/tools/`**

- **Tool Documentation:** `tool-{id}.md` files contain tags for `tool-name` and `usage via elisp`, documenting how to utilize each tool within Emacs.

- **`shared/prompts/`**

- **Prompt Templates:** `prompt-template.md` uses tags like `why`, `what`, `how`, ensuring prompts are comprehensive and consistent.

#### **Projects**

- **Project-Level README.md:** Include tags such as `overview`, `objectives`, and `team members` to provide a quick summary.

- **Data Management:**

- **Data README.md:** Document the structure and format of datasets, facilitating data handling and preprocessing.

- **Outputs and Reports:**

- **Reports README.md:** Outline the contents of reports, changes since the last version, and key achievements.

- **Version Control:**

- **Environment Files:** Including `.env` files allows for environment-specific configurations, aiding in reproducibility.

#### **Ninjas (Agents)**

- **Profile and Status:**

- **`profile.json`:** Contains structured data about the ninja, such as `name`, `roles`, and `skills`.
- **`status.md`:** Uses tags to indicate current tasks (`todo`, `in-progress`, `done`), enhancing task tracking.

- **Memory and Journals:**

- **Time-Stamped Entries:** Files like `YYYY-MM-DD-<title>.md` in `memory/` capture daily thoughts, ideas, successes, and failures, facilitating reflection and knowledge accumulation.

- **Messages:**

- **Communication Logs:** While currently placeholders, adding `inbox/` and `outbox/` directories under `messages/` will organize correspondence.

- **Individual Projects:**

- **Project-Specific README.md:** Tags cover comprehensive project details, aiding in personal project management.

- **Forum and Collaboration:** `forum.md` can serve as a space for discussion and collaboration notes relevant to the ninja's involvement in the project.

---

### **Optimizing the Tagging System**

- **Consistent Tag Usage:** Ensure tags are consistently applied across all files to enhance searchability.

- **Metadata Headers:**

- In Markdown files, include metadata at the top:

```markdown
---
title: "README"
tags: [project-{id}, overview, assignments]
---
```

- **Leverage Emacs Packages:**

- Utilize packages like `helm` or `org-mode` for tag-based navigation and querying within the workspace.

---

### **Benefits of This Structure**

- **Simplicity:** The hierarchy is straightforward, reducing cognitive load when navigating the workspace.

- **Maintainability:** Organized directories with clear documentation make it easy to update and maintain over time.

- **Scalability:** The structure supports growth in the number of projects and agents without requiring significant changes.

- **Readability:** README files and tagged metadata provide context, making it easy to understand the purpose of each component.

- **Usability:** Tailored for use within Emacs, leveraging features like text editing, search, and script integration.

- **Self-Evolving:** The inclusion of templates and a tagging system allows the workspace to adapt as needs change, supporting continuous improvement.

---

### **Conclusion**

Your proposed workspace structure effectively balances the need for organization with the flexibility required for a self-evolving multi-agent system in Emacs. By emphasizing simplicity and readability through consistent use of README files and tags, you create an environment that is both easy to navigate and maintain.

Incorporating templates and standardized documentation practices fosters scalability and usability, ensuring that as new projects and agents are added, they integrate smoothly into the existing structure. The focus on text-based files aligns perfectly with Emacs' capabilities, allowing you to leverage powerful editing and search functionalities.

Overall, this structure supports your goals and positions your project for efficient development and growth.


# Templates
## **1. `shared/agents/templates/agent-template.md`**

```markdown
---
title: "[Agent Name] Template"
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

[Include any additional information relevant to the agent.]
```

**Usage Example:**

```markdown
---
title: "Data Analysis Agent Template"
available tags: [agent-template, data-analysis, Python, statistics]
---

# Agent Template: Data Analysis Agent

## **Role**

- **Primary Role:** Analyze datasets to extract meaningful insights.
- **Responsibilities:**
- Perform statistical analyses.
- Generate visualizations.
- Prepare reports summarizing findings.

## **Available Tools**

- Elisp functions
- Python functions
- ...

## **Expertise**

- Statistical Modeling
- Data Cleaning
- Visualization

## **Communication Protocols**

- **Preferred Method:** Direct Messages

## **Additional Notes**

Available for consultation on data preprocessing techniques.
```

---

## **2. `shared/prompts/prompt-template.md`**

```markdown
---
title: "[Prompt Title]"
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

[Include any other relevant information.]
```

**Usage Example:**

```markdown
---
title: "Data Cleaning Prompt"
available tags: [prompt, data-cleaning, dataset-123, CSV, pandas, expected-output, summary-report]
---

# Data Cleaning Prompt

(PLEASE FILL THIS TEMPLATE)
```

---

## **3. `ninjas/ninja-{id}/profile.json`**

```json
{
"ninja_id": "ninja-001",
"nickname": "Nickname",
"role": "[Role in the project]",
"skills": ["Skill1", "Skill2", "Skill3"],
"assigned_projects": ["project-001", "project-002"],
"tags": ["ninja-profile", "agent", "expertise-area"]
}
```

**Usage Example:**

```json
{
"ninja_id": "ninja-001",
"name": "Alex Smith",
"role": "Data Scientist",
"skills": ["Python", "Machine Learning", "Data Visualization"],
"projects": ["project-001", "project-003"],
}
```

---

## **4. `ninjas/ninja-{id}/status.md`**

```markdown
---
title: "Status Update"
available tags: [ninja-status, todo, waiting, pending, done, canceled, reason]
date: YYYY-MM-DD
---

# Status Update - [YYYY-MM-DD]

## **Current Status**

- **Tasks Todo:**
- [Task 1](../../projects/project-{id}/tasks/task-001.md)
- [Task 2](../../projects/project-{id}/tasks/task-002.md)
- **In Progress:**
- [Task 3](../../projects/project-{id}/tasks/task-003.md)
- **Completed:**
- [Task 4](../../projects/project-{id}/tasks/task-004.md)

## **Notes**

- Waiting for data from [Team Member].
- Pending approval on [Report](../../projects/project-{id}/outputs/report.pdf).

## **Reason**

No blockers at this time.

## **Next Steps**

- Continue working on data analysis.
- Prepare visualization for the presentation.
```

---

## **5. `projects/project-{id}/README.md`**

```markdown
---
title: "Project {id} Overview"
available tags: [project description, aim, assignments, timeline, milestones, resources]
---

# Project {id} - [Project Title]

## **Aim**

[State the main objective of the project.]

## **Project Description**

[Provide a detailed description of the project.]

## **Assignments**

- **Team Members:**
- [Ninja 1](../../ninjas/ninja-001/profile.json)
- [Ninja 2](../../ninjas/ninja-002/profile.json)
- **Roles:**
- Ninja 1: Data Collection
- Ninja 2: Data Analysis

## **Timeline**

- **Start Date:** YYYY-MM-DD
- **End Date:** YYYY-MM-DD

## **Milestones**

1. [Milestone 1] - Due: YYYY-MM-DD
2. [Milestone 2] - Due: YYYY-MM-DD

## **Additional Resources**

- [Data README](data/README.md)
- [Documentation](docs/README.md)
```

**Usage Example:**

```markdown
---
title: "Project 001"
available tags: [project description, aim, assignments, timeline, milestones, resources]
---

# Project 001 - Customer Behavior Analysis

## **Project Description**

This project focuses on analyzing transactional data to identify patterns in customer purchasing behavior, which will inform the development of a new recommendation engine.

## **Aim**

To analyze customer purchasing behavior to improve product recommendations.

## **Assignments**

- **Team Members:**
- [Alex Smith](../../ninjas/ninja-001/profile.json)
- [Jordan Lee](../../ninjas/ninja-002/profile.json)
- **Roles:**
- Alex Smith: Data Extraction and Cleaning
- Jordan Lee: Data Modeling and Visualization

## **Timeline**

- **Start Date:** 2023-10-01
- **End Date:** 2023-12-31


## **Milestones**

1. Data Collection Complete - Due: 2023-10-15
2. Data Cleaning Complete - Due: 2023-10-31
3. Preliminary Analysis - Due: 2023-11-15
4. Final Report - Due: 2023-12-31

## **Resources**

- [Data README](data/README.md)
- [Documentation](docs/README.md)
```

---

## **6. `ninjas/ninja-{id}/memory/YYYY-MM-DD-<title>.md`**

```markdown
---
title: "[Entry Title]"
available tags: [summary, success, failure, tools utilized, data referenced, ideas, next steps]
date: YYYY-MM-DD
---

# [Entry Title]

## **Summary**

[Provide a brief summary of the day's activities or thoughts.]

## **Details**

- **Successes:**
- [Describe any successes achieved.]
- **Failures:**
- [Describe any challenges or failures.]
- **Tools Utilized:**
- [Tool 1](../../../shared/tools/tool-001.md)
- **Data Referenced:**
- [Dataset](../../../projects/project-{id}/data/dataset.csv)
- **Ideas:**
- [Outline any new ideas or insights.]

## **Next Steps**

[Plan for the following day or tasks.]
```

**Usage Example:**

```markdown
---
title: "Initial Data Exploration"
available tags: [summary, success, failure, tools utilized, data referenced, ideas, next steps]
date: 2023-10-05
---

# Initial Data Exploration

## **Summary**

Today, I began exploring the customer transaction dataset for Project 001.

## **Details**

- **Successes:**
- Successfully loaded and previewed the dataset.
- Identified key variables for analysis.
- **Failures:**
- Encountered issues with missing values in the `purchase_amount` column.
- **Tools Utilized:**
- [Pandas](../../../shared/tools/tool-pandas.md)
- **Data Referenced:**
- [Customer Transactions Dataset](../../../projects/project-001/data/dataset.csv)
- **Ideas:**
- Implement a data imputation strategy for missing values.
- Consider feature engineering on the `purchase_date`.

## **Next Steps**

- Address missing values in the dataset.
- Start preliminary data analysis and visualization.
```

---

## **7. Messages Template: `YYYY-MM-DD-{subject}-from-<from>-to-<to>.md`**

```markdown
---
subject: "[Message Subject]"
from: "[Sender Name]"
to: "[Recipient Name]"
cc: [Optional CC list]
date: YYYY-MM-DD
available tags: [subject, from, to, cc, date, message, attachments, references]
---

# [Message Subject]

## **Message**

[Write the message content here.]

## **Attachments**

- [Attachment 1](path/to/attachment)

## **References**

- [Related Document](path/to/document)
```

**Usage Example:**

```markdown
---
subject: "Data Issues Found"
from: "Alex Smith"
to: "Jordan Lee"
cc:
date: 2023-10-06
available tags: [subject, from, to, cc, date, message, attachments, references]
---

# Data Issues Found

## **Message**

Hi Jordan,

I noticed that there are several missing values in the `purchase_amount` column of the dataset. We need to decide on an appropriate method to handle these missing entries. Do you have any suggestions?

Best,
Alex

## **Attachments**

- [Data Summary Report](../../reports/data-summary-2023-10-06.pdf)

## **References**

- [Dataset README](../../../projects/project-001/data/README.md)
```

---

## **8. `shared/tools/tool-{id}.md`**

```markdown
---
title: "[Tool Name]"
available tags: [tool name, description, elisp command, examples]
---

# Tool: [Tool Name]

## **Description**

[Provide a brief description of the tool.]

## **Usage**

## **Elisp Command**
``` emacs-lisp
(progn
  (command1 arg1 arg2)
  (command2 arg1 arg2)
  ...)
```

## **Examples**

- **Example 1:**

``` emacs-lisp
(progn
(setq default-directory "/workspace/")
(delete-other-windows)
(split-window-right)
(let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
(script-filename (expand-file-name (format "plot-%s.py" timestamp) default-directory))
(image-filename (expand-file-name (format "plot-%s.png" timestamp)))
(py-code "
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
"))
    (with-temp-buffer
(insert (replace-regexp-in-string "image-file" image-filename py-code))
(write-region (point-min) (point-max) script-filename)
(shell-command (format "bash -c 'source /workspace/.env/bin/activate && python3 %s'" script-filename)))
    (find-file script-filename)
    (sleep-for 3)
    (other-window 1)
    (find-file (expand-file-name image-filename default-directory))
    (sleep-for 3)))
```


## **Additional Information**

[Include any other relevant details or links.]
```

---

## **9. Leveraging Tags in Emacs**

To maximize the efficiency of your tag system within Emacs:

- **Enable Org Mode Tags:** Utilize Org mode's tagging system for organizing and searching through your notes and files.

- **Use Helm or Ivy Counsel:** These packages enhance Emacs' completion features, allowing you to search files by tags.

- **Custom Scripts:** Write Emacs Lisp functions to parse the YAML front matter in your `.md` files, enabling custom searches and navigation.

**Example Emacs Lisp Function to Extract Tags:**

```emacs-lisp
(defun extract-tags-from-front-matter ()
"Extract tags from YAML front matter in the current buffer."
(interactive)
(goto-char (point-min))
(when (search-forward-regexp "^available tags: \\[\\(.*?\\)\\]" nil t)
(message "available tags: %s" (match-string 1))))
```

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
    *   **Agent Profiles:**  `ninjas/ninja-{id}/profile.json` is excellent for structured data. You could expand this.
    *   **Tool Definitions:**  `shared/tools/tool-{id}.json` could hold structured information about tools, including required inputs, outputs, and elisp code snippets, and maybe add a JSON schema for validation.
    *   **Knowledge Base:** `shared/knowledge_base/knowledge.json` is a good place for structured information for agents.
    *   **Prompts**: `shared/prompts/prompt-{id}.json` can hold structured information about the prompt.
    *    **Task Management**: Task lists for ninjas in `ninjas/ninja-{id}/tasks.json`

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
└── ninjas/
    ├── README.md
    └── ninja-{id}/
        ├── profile.json (tags: ninja name in project, role)
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
