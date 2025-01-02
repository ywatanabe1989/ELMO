<!-- ---
!-- title: ./ELMO/docs/dev_memo/permissions_.md
!-- author: ywatanabe
!-- date: 2024-12-16 19:46:17
!-- --- -->


# Permissions
## Server-auth-key

``` elisp
(setq server-auth-key "shared-secret") ; Set a shared key for all users
(setq server-allow-remote-clients t) ; Allow remote connections 
(setq server-host "0.0.0.0") ; Listen on all interfaces
(setq server-raise-frame nil) ; Don't auto-raise frames
(setq server-window #'pop-to-buffer) ; Open in new windows
(setq server-visit-hook
      (lambda ()
        (set-file-modes (buffer-file-name) #o666))) ; Share file access
(length server-clients)

```

## Read-only for sensitive files
```elisp
;; Make sensitive files read-only for agents
(setq server-visit-hook
      (lambda ()
        (when (string-match-p "sensitive/" (buffer-file-name))
          (set-file-modes (buffer-file-name) #o444)))) ; r--r--r--

;; Limit which directories/files agents can access
(setq server-allowed-dirs '("/path/to/allowed/dir"))
```

```elisp
;; In your init.el or test config
(setq server-directory-permissions #o775)
(setq server-auth-key "test-secret-key")
(setq server-allow-remote-clients t)

;; For sensitive files
(setq server-visit-hook
      (lambda ()
        (when (string-match-p "sensitive/" (buffer-file-name))
          (set-file-modes (buffer-file-name) #o444))))

;; Test script
(defun test-server-access ()
  (server-start)
  (message "Server started at %s" server-socket-dir)
  (let ((test-file "/tmp/test-file.txt"))
    (with-temp-file test-file 
      (insert "test content"))
    (set-file-modes test-file #o664)))
```

