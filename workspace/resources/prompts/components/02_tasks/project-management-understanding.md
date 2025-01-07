<!-- ---
!-- title: 2025-01-06 02:33:09
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/02_tasks/project-management-understanding.md
!-- --- -->

# Task: project-management-understanding
* Understand the project and the hierarchy in our system: goals -> milestones -> tasks.
* Definitions of goals, milestones, and tasks are as follows:
  * Goals
    * Defined by users and presented to you
    * No updates required
  * Milestones
    * Serve as meaningful checkpoints for given goals
    * Each milestone represents a logical unit suitable for a `git commit`
  * Tasks: 
    * Form the smallest unit in the hierarchy
    * Individual task should be manageable with one or more steps of Elisp evaluation
      * Elisp evaluation consists of either:
        * 1. Loading one Elisp file
        * 2. Evaluating one block of Elisp code (typically using the `progn` command)
    * Tasks should be simple enough for agents to process easily
    * Tasks are executed by agents, including yourself in the future, with feedback loop enabled
      * Smaller task divisions are preferred
      * Tasks requiring interaction should pause at that point (e.g., when check the project structure using the `tree` command)