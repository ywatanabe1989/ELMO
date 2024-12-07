<!-- ---
!-- title: ./self-evolving-agent/src/docs/working_with_elisp.md
!-- author: ywatanabe
!-- date: 2024-12-06 02:50:15
!-- --- -->


# Working with Elisp

``` elisp
(sea-exec-elisp-code (sea-escape-elisp-code '(message "hi")))

(sea-exec-elisp-code "'\(message \"Hello SEA!!!\"\)'")

(sea-exec-elisp-code
 "'\(progn
    (find-file \"/tmp/test.txt\")
    (insert \"This is a test script.\")
  \)'")
  
(sea-exec-elisp-code 
  "'\(message \"%s\" \(\+ 2 3\)\)'")
```
