;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 04:59:57
;;; Time-stamp: <2025-01-05 04:59:57 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/run-project.el

(defun llemacs-run (prompt &optional recipe-id log-level)
  "Run LLM agent with PROMPT, RECIPE-ID, and LOG-LEVEL.
RECIPE-ID defaults to code-gen.
LOG-LEVEL defaults to error."
  (interactive "sPrompt: ")
  (let* ((recipe-id (or recipe-id "code-gen"))
         (log-level (or log-level "error"))
         (context (llemacs--pj-collect-context (llemacs--pj-get-cur-pj) log-level))
         (embedded-prompt (llemacs--llm-prompt-embed prompt recipe-id))
         (full-prompt (concat context embedded-prompt)))
    (llemacs--run-prompt full-prompt)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))