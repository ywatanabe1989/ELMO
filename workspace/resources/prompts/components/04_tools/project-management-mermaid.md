<!-- ---
!-- Timestamp: 2025-01-09 19:12:49
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/04_tools/project-management-mermaid.md
!-- --- -->

# Tool: project-management-mermaid
To generate images from the mermaid file for the project, use this function.
``` elisp
(defun llemacs--pj-mermaid-compile ()
  "Compiles project management mermaid diagram into image files.
  Processes /project_management/project_management.mmd in current project
  ('llemacs--path-pj-pm-mmd'), generating corresponding
  PNG, SVG, and GIF files."
```
