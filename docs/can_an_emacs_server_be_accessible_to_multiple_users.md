``` bash
#!/bin/bash
# test_emacs_server.sh

export HOST_USER=ywatanabe
export SEMACS_USER=semacs
export SERVER="/home/ywatanabe/.dotfiles/.emacs.d/server/server"

# Setup SEMACS_USER
sudo useradd $SEMACS_USER
sudo passwd $SEMACS_USER
sudo mkdir -p /home/$SEMACS_USER
sudo chown $SEMACS_USER:$SEMACS_USER /home/$SEMACS_USER

# Set permissions
sudo chmod 755 $(dirname $SERVER)
sudo chmod 777 $SERVER
sudo usermod -a -G $HOST_USER $SEMACS_USER

# Restart Emacs server with proper permissions
emacsclient -e '(kill-emacs)'
emacs --daemon --eval '(setq server-directory-permissions #o775)'

# Test access
su - $SEMACS_USER -c "ls -al $(dirname $SERVER)"
su - $SEMACS_USER -c "emacsclient --socket-name=$SERVER -e '(message \"Test from SEMACS_USER\")'"

# Check server status
ps aux | grep "emacs.*daemon"
lsof -i | grep emacs
```

Based on the source code, here are the key insights for multi-user server access:
================================================================================

1. Socket Permissions:
```elisp
;; /opt/emacs-29.4/share/emacs/29.4/lisp/server.el.gz

(setq server-directory-permissions #o777) ; Allow all users to access
(setq server-socket-dir "/tmp/emacs-shared") ; Use shared directory
```

2. Authentication:
```elisp
(setq server-auth-key "shared-secret") ; Set a shared key for all users
```

3. Process Environment:
```elisp
(setq server-allow-remote-clients t) ; Allow remote connections 
(setq server-host "0.0.0.0") ; Listen on all interfaces
```

4. Multiple Frames:
```elisp
(setq server-raise-frame nil) ; Don't auto-raise frames
(setq server-window #'pop-to-buffer) ; Open in new windows
```

5. File Access:
```elisp
(setq server-visit-hook
      (lambda ()
        (set-file-modes (buffer-file-name) #o666))) ; Share file access
```

The key is properly setting permissions and authentication to allow multiple users while maintaining security. The server can handle multiple concurrent clients through `server-clients` tracking.

Let me know if you need specific implementation details.


For AI agents, you can set up restricted access like this:
================================================================================

```elisp
;; Set restrictive directory for AI agents
(setq server-socket-dir "/path/to/ai-agents-dir")
(set-file-modes server-socket-dir #o755) ; rwxr-xr-x

;; Make sensitive files read-only for agents
(setq server-visit-hook
      (lambda ()
        (when (string-match-p "sensitive/" (buffer-file-name))
          (set-file-modes (buffer-file-name) #o444)))) ; r--r--r--

;; Limit which directories/files agents can access
(setq server-allowed-dirs '("/path/to/allowed/dir"))
```

This gives agents read/execute access but prevents writing to sensitive files. You should also implement additional validation/sandboxing for AI agent commands.


Here's the combined test code:
================================================================================
```elisp
;; In your init.el or test config
(setq server-name "shared-server"
      server-socket-dir "/tmp/emacs-shared"
      server-directory-permissions #o775  ; rwxrwxr-x
      server-auth-key "test-secret-key"
      server-allow-remote-clients t)

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

Test with:
```bash
# As ywatanabe
emacs --daemon --load test-config.el

# As semacs
export EMACS_SERVER_FILE=/tmp/emacs-shared/shared-server
emacsclient -e '(message "Connected as semacs")'
```

Yes, you can create a monitoring buffer. Here's a basic implementation:
================================================================================
(defvar agent-monitor-buffer "*Agent Monitor*")

(defun agent-monitor-create ()
  (interactive)
  (with-current-buffer (get-buffer-create agent-monitor-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (special-mode)))

(defun agent-monitor-update ()
  (when (get-buffer agent-monitor-buffer)
    (with-current-buffer agent-monitor-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "=== Agent Status ===\n\n")
      (dolist (client server-clients)
        (insert (format "Agent: %s\n" (process-get client 'env)))
        (dolist (buf (process-get client 'buffers))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (insert (format "  Buffer: %s\n    Position: %d\n    Last command: %s\n\n"
                            (buffer-name) (point) last-command))))))
      (read-only-mode 1))))

;; Update every second
(run-with-timer 0 1 #'agent-monitor-update)

Yes, you can highlight other agents' cursors. Here's how:
================================================================================

```elisp
(defvar-local agent-cursors-overlays nil)

(defun highlight-agent-cursor (agent-pos color)
  (let ((ov (make-overlay agent-pos (1+ agent-pos))))
    (overlay-put ov 'face `(:background ,color))
    (overlay-put ov 'agent-cursor t)
    (push ov agent-cursors-overlays)))

(defun update-agent-cursors ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      ;; Clear old highlights
      (dolist (ov agent-cursors-overlays)
        (delete-overlay ov))
      (setq agent-cursors-overlays nil)
      ;; Add new highlights
      (dolist (client server-clients)
        (when (eq buf (process-get client 'current-buffer))
          (highlight-agent-cursor 
           (process-get client 'cursor-pos)
           (process-get client 'cursor-color)))))))

;; Update cursor positions periodically
(run-with-timer 0 0.5 #'update-agent-cursors)
```
