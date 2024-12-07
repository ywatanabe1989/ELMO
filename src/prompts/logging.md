<!-- ---
!-- title: ./self-evolving-agent/src/prompts/logging.md
!-- author: ywatanabe
!-- date: 2024-12-05 22:38:58
!-- --- -->


## Logging for the future
- Use the log file (`/home/ywatanabe/.sea/logs/history.log`)
- The log file is the medium to communicate among me, you, and you in the future
- For logging, use these elisp functions
- (defun sea--log-success (_message)
  "Log success MESSAGE."
  (interactive)
  (sea--log-message (concat "[SUCCESS] " (message "%s" _message))))
- (defun sea--log-warning (_message) ...
- (defun sea--log-error (_message) ...
- (defun sea--log-note (_message) ...
