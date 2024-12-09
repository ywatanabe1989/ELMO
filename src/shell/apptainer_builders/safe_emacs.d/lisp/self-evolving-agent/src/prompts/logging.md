<!-- ---
!-- title: ./self-evolving-agent/src/prompts/logging.md
!-- author: ywatanabe
!-- date: 2024-12-04 08:48:47
!-- --- -->


## Logging for the future
- The log file will be provided when necessary.
- You should use the logging system for future use.
- For logging, these functions are prepared.
- (defun sea--log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (sea--log-message (concat "[SUCCESS] " (message "%s" _message))))

- (defun sea--log-warning (_message) ...
- (defun sea--log-error (_message) ...
- (defun sea--log-note (_message) ...
- (defun sea--get-log (&optional n_tail)
  "Read last N_TAIL lines from agent log file. Default is 32 lines."
  (interactive) ...
