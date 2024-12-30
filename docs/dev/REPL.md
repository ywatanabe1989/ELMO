<!-- ---
!-- title: 2024-12-28 17:39:08
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/llemacs/docs/dev/REPL.md
!-- --- -->

1. REPL cycle
   **Read**
   - Takes input from user/system
   - Parses into internal representation

   **Evaluate**
   - Processes the parsed input 
   - Executes computations/logic

   **Print**
   - Shows results
   - Formats output

   **Loop**
   - Returns to Read step
   - Maintains state between iterations
2. Model-View-Controller (MVC)
   - Context = Model
   - LLM = Controller
   - Execution/Feedback = View

3. Unix Philosophy
   - Small, focused components
   - Input/Output streams
   - Composability

4. CI/CD Pipeline structure
   - Context = Environment setup
   - LLM = Build
   - Execution = Deploy
   - Feedback = Monitor

**Unit (or Step):**
| Unit          | Description                                                         |
|---------------|---------------------------------------------------------------------|
| [ ] **Context**   | Project, Tasks, Progress, Status, Past log, resources               |
| [x] **LLM**       | Translates Context to Code                                          |
| [x] **Execution** | Executes Code, generating Output                                    |
| [ ] **Feedback**  | Produced outputs, Stdout, Stderr, and Exit codes, to update context |