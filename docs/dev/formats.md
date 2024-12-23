<!-- ---
!-- title: ./ELMO/docs/dev_memo/formats.md
!-- author: ywatanabe
!-- date: 2024-12-18 14:11:25
!-- --- -->


## Strict JSON Formats

### Context

```json
{
  "project_id": "string",
  "milestone_id": "string",
  "task_id": "string",
  "process_id": "string",
  "thread_id": "string",
    "branch_id": "string",
  "description": "string",
  "resources": [
    {
      "type": "file | link | data",
      "id": "string",
       "path": "string",
      "access": "read | write"
    }
  ],
  "tags": ["string"],
    "created_at": "timestamp",
   "updated_at": "timestamp",
    "status": "ready | running | pending | finished",
    "parent_context": {
        "project_id": "string",
       "milestone_id": "string",
        "task_id": "string"
    }
}
```

### LLM Prompt Template

```json
{
  "template_id": "string",
  "version": "integer",
  "description": "string",
  "template": "string",
    "variables": ["string"],
  "created_at": "timestamp",
   "updated_at": "timestamp"
}
```

### Tool Definition

```json
{
  "tool_id": "string",
  "type": "elisp | script",
  "description": "string",
  "command": "string",
  "input_schema": {
    "type": "object",
    "properties": {
      "arg1": { "type": "string" },
      "arg2": { "type": "integer" }
    },
    "required": ["arg1"]
  },
  "output_schema": {
    "type": "object",
    "properties": {
      "result": { "type": "string" },
      "status": { "type": "integer"}
    }
  },
  "created_at": "timestamp",
   "updated_at": "timestamp"
}
```

### Feedback

```json
{
  "execution_id": "string",
  "stdout": "string",
  "stderr": "string",
  "exit_code": "integer",
    "created_at": "timestamp"
}
```

### Message

```json
{
  "message_id": "string",
  "type": "log | command | event",
  "sender": "string",
  "recipient": "string",
  "timestamp": "timestamp",
  "payload": {
    "type": "string",
    "content": "string | object"
  },
    "status": "sent | read | junk"
}
```
```

### Project Definition

```json
{
  "project_id": "string",
  "name": "string",
  "description": "string",
  "created_at": "timestamp",
  "updated_at": "timestamp",
  "status": "ready | running | pending | finished",
    "milestones": ["string"],
  "tags": ["string"]
}
```

### Agent Configuration

```json
{
  "agent_id": "string",
  "name": "string",
  "description": "string",
  "image": "string",
  "resources": ["string"],
  "tools": ["string"],
  "created_at": "timestamp",
    "updated_at": "timestamp",
  "status": "ready | running | pending | finished",
    "current_task": "string"
}
```

### Resource Definition

```json
{
  "resource_id": "string",
  "name": "string",
    "type": "file | link | data",
  "description": "string",
  "path": "string",
  "access": "read | write",
  "created_at": "timestamp",
  "updated_at": "timestamp",
  "tags": ["string"]
}
```

### Knowledge Entry

```json
{
  "knowledge_id": "string",
  "type": "text | json | link",
  "content": "string | object",
    "summary": "string",
  "created_at": "timestamp",
    "updated_at": "timestamp",
  "tags": ["string"],
    "source": "string"
}
```

### Experience Log

```json
{
 "log_id": "string",
  "agent_id": "string",
  "task_id": "string",
  "start_time": "timestamp",
  "end_time": "timestamp",
  "status": "success | failure",
  "feedback_id": "string",
  "created_at": "timestamp",
  "updated_at": "timestamp"
}
```
