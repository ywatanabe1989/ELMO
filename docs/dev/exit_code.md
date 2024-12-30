<!-- ---
!-- title: 2024-12-28 10:24:46
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/llemacs/docs/dev/exit_code.md
!-- --- -->

Python:
- 0: Success
- 1: General errors
- 2: Command line syntax errors
- >2: Program-specific error codes

Bash:
- 0: Success
- 1-255: Error codes
- Common ones:
  1: General errors
  2: Misuse of shell builtins
  126: Command invoked cannot execute
  127: Command not found
  128+n: Fatal error signal "n"
  130: Script terminated by Ctrl+C

Elisp:
- No built-in exit codes
- Functions typically return:
  nil: Failure/false
  t: Success/true
  Or custom values defined by function
- Errors are handled via error signals and condition-case