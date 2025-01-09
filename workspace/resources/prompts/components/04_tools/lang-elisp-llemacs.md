<!-- ---
!-- Timestamp: 2025-01-09 18:06:46
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/04_tools/lang-elisp-llemacs.md
!-- --- -->
<!-- ---
!-- title: 2025-01-06 11:24:08
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/07_tools/elisp-llemacs.md
!-- --- -->

* Logging System
   - Tools: `llemacs--logging-write-*-pj`
   - Input: Message string
   - Output: Log entry in appropriate file
   - Variants: debug, info, success, search, elisp, prompt, api, warn, error
   - Example: `(llemacs--logging-write-error-pj "Error message")`

* Mermaid Diagram Management
   - Tool: `my/mermaid-compile`
   - Input: Mermaid file (.mmd)
   - Output: PNG, SVG, GIF versions
   - Location: `llemacs--path-pj-pm-mmd`

4. Buffer Display
   - Tool: `llemacs--buf-disp-main-pj`
   - Input: File path, display options
   - Output: Displayed buffer
   - Example: `(llemacs--buf-disp-main-pj "file.txt" t t t)`

5. Python Environment
   - Location: `llemacs--path-python-sys`
   - Input: Python scripts
   - Output: Results in `llemacs--path-pj-results`
   - Required: argparse implementation

6. Project Structure
   - Root: `llemacs--path-pj`
   - Data: `llemacs--path-pj-data`
   - Scripts: `llemacs--path-pj-scripts`
   - Results: `llemacs--path-pj-results`
   - Logs: `llemacs--path-pj-logs`
