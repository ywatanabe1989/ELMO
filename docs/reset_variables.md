<!-- ---
!-- Timestamp: 2025-01-10 22:53:45
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/docs/reset_variables.md
!-- --- -->

``` elisp
(progn 
(mapatoms
 (lambda (symbol)
   (when (and (boundp symbol)
              (string-prefix-p "llemacs--" (symbol-name symbol)))
     (makunbound symbol))))

;; (makunbound 'llemacs--llm-anthropic-key)
;; (makunbound 'llemacs--llm-google-key)
;; (makunbound 'llemacs--llm-deepseek-key)
;; (load-file (expand-file-name "~/.emacs.d/init.el"))
(llemacs--update-docs)
(llemacs--llm-prompt-compile "code-elisp-progn")
(llemacs--llm-prompt-compile "project-management")
)

cat workspace/resources/prompts/components/04_tools/llemacs-variables.md | grep anthropic -3
cat workspace/resources/prompts/components/04_tools/llemacs-variables.md | grep google -3
```

