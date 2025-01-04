<!-- ---
!-- title: 2025-01-05 01:44:06
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/examples/automatic-project-development.md
!-- --- -->

# An example usage of Llemacs: local without using Apptainer

## Automatic project development

``` elisp
;; Initialize a project: setting project-name and goals
(llemacs--proj-init 
    "dsp-project" 
    "- Calculate features from EEG demo data for epilepsy detection and prediction
     - Implement ML models
     - Evaluate the performances
")

;; Advance the project by LLM agents
(defun llemacs-run (prompt &optional recipe-id log-level)
  "Run LLM agent with PROMPT, RECIPE-ID, and LOG-LEVEL.
RECIPE-ID defaults to code-gen.
LOG-LEVEL defaults to error."
  (interactive "sPrompt: ")
  (let* ((recipe-id (or recipe-id "code-gen"))
         (log-level (or log-level "error"))
         (context (llemacs--proj-collect-context (llemacs--pj-get-cur-pj) log-level))
         (embedded-prompt (llemacs--llm-prompt-embed prompt recipe-id))
         (full-prompt (concat context embedded-prompt)))
     (llemacs--run-prompt full-prompt)))


(llemacs-run "advance the project")
(llemacs-run "calculate DMD")
(llemacs-run "calculate PAC")
(llemacs-run "calculate PAC; ensure results are saved in valid directories")


org-display-inline-images
org-redisplay-inline-images

(llemacs--run-prompt (llemacs--llm-prompt-embed "prompt" "code-gen")) ;; this works
    
```

## Basics
``` elisp
;; Run Elisp code with automatic logging
;; (llemacs--run-elisp `(message "hi"))

;; Compile prompt template (recipe-id: "code-gen")
;; (llemacs--llm-prompt-compile "code-gen")

;; Embed the prompt into template
;; (llemacs--llm-prompt-embed "plot something" "code-gen")

;; Translate prompt to Elisp code and evaluate
;; (llemacs--run-prompt "plot something" "code-gen")

;;;; Switch LLM Provider
;;(llemacs--llm-switch-provider "anthropic")
;;(llemacs--llm-switch-provider "google")
```