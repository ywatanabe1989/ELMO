<!-- ---
!-- title: 2025-01-06 11:14:01
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/llm-prompt-engineering.md
!-- --- -->

# Task: llm-prompt-engineering
* Understand:
  * The user requests
  * Expected inputs
  * Expected outputs
  * The prompt assembly schema in this system:
    * `llemacs--path-res-prompt-compiled`
      * Includes compiled prompts as markdown files
    * `(exapand-file-name "roles" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "rules" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "tasks" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "examples" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "tools" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "resources" llemacs--path-res-prompt-components)`
    * `(exapand-file-name "requests" llemacs--path-res-prompt-components)`
      * Includes compiled prompts as markdown files
    * `llemacs--path-res-prompt-recipes`
      * Includes compiled prompts as markdown files

* Develop prompt with the following workflow:
  * Search existing compiled prompts
  * If none found:
    * Create recipe combining:
       * Roles
       * Tasks
       * Rules
       * Examples
       * Resources
       * Requests
     * Develop new components as needed



    