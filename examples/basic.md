<!-- ---
!-- title: 2025-01-05 15:51:53
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/examples/basic.md
!-- --- -->

## Basics
``` elisp
;; Run Elisp code with automatic logging
(llemacs--run-elisp `(message "hi"))

;; Compile prompt template (recipe-id: "code-gen")
(llemacs--llm-prompt-compile "code-gen")

;; Embed the prompt into template
(llemacs--llm-prompt-embed "plot something" "code-gen")

;; Translate prompt to Elisp code and evaluate
(llemacs--run-prompt "plot something" "code-gen")

;;;; Switch LLM Provider
(llemacs--llm-switch-provider "anthropic")
(llemacs--llm-switch-provider "google")
(llemacs--llm-switch-provider "deepseek")
```