;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 20:26:57
;;; Timestamp: <2025-01-09 20:26:57>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/_05-run-project.el


(defun llemacs-run-project (prompt &optional recipe-id log-level)
  "Run LLM agent with PROMPT, RECIPE-ID, and LOG-LEVEL.
RECIPE-ID defaults to code-gen.
LOG-LEVEL defaults to error."
  (interactive "sPrompt: ")
  (condition-case err
      (let* ((recipe-id (or recipe-id "code-gen"))
             (log-level (or log-level "error"))
             (context (llemacs--pj-collect-context (llemacs--pj-get-cur-pj) log-level))
             (embedded-prompt (llemacs--llm-prompt-embed prompt recipe-id))
             (full-prompt (concat context embedded-prompt)))
        (llemacs--run-prompt full-prompt))
    (llemacs--logging-write-error-pj"Run failed: %S" err)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
