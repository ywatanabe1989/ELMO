<!-- ---
!-- title: 2025-01-03 02:51:35
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompt-templates/components/03_rules/code-logging.md
!-- --- -->

# Rules: code-logging
* Log important points using:
  `(defun llemacs--logging-log-debug (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-info (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-success (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-search (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-elisp (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-prompt (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-api (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-warn (message &optional project-id-or-full-name))`
  `(defun llemacs--logging-log-error (message &optional project-id-or-full-name))`
* Provide meaningful error messages