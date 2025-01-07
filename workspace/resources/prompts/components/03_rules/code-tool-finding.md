<!-- ---
!-- title: 2025-01-06 12:03:35
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-tool-finding.md
!-- --- -->

## Rule: code-tool-finding
* Search Order is as follows:
   - Custom before anything
   - Local before remote
   - Official before community
   - Simple before complex

* Documentation Priority is as follows:
   - Built-in help/man > Docstrings > README > External docs > Community docs

* Consider resource constraints

* Output Format is as follows:
  * Save as a markdown file under `(expand-file-name "tools" llemacs--path-res-prompt-components`)
  * File name should be: <category>-<tool-name>-<tool-type>
    ## Filename
    - Name: [tool name]
    - Category: [lang|sys|shell]
    - Type: [custom|external|package]
  * File contents should follow this template. Unnecessary entries should be skipped.
    ```markdown
    ## Installation
    - Installed: Yes/No
    - Installation guide (if not installed): 
      ```markdown
      (Installation guide here)
      ```
    ### Input
    - Format: [specification]
    - Required parameters: [list]
    - Optional parameters: [list]
    ### Output
    - Format: [specification]
    - Location: [path/variable]
    - Success indicators: [description]
    ### Example
    ```[language]
    [example code]
    ```