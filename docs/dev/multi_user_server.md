``` bash
#!/bin/bash
# test/basic_server.sh

export APPTAINERENV_NINJA_SERVER_SOCKET_DIR="/tmp/ninja-shared"
export APPTAINERENV_NINJA_SERVER_SOCKET=$NINJA_SERVER_SOCKET_DIR/emacs/server
ashell_sw
echo $NINJA_SERVER_SOCKET
# First stop the current daemon
emacsclient -e '(kill-emacs)'

# Try starting daemon again
su - ninja-001 -c "emacs --daemon=$NINJA_SERVER_SOCKET"
```


``` elisp
;; src/ninja-monitor.el
(defun ninja-monitor-start ()
  (interactive)
  (let ((buf (get-buffer-create "*NINJA-MONITOR*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== NINJA Monitor ===\n")
      (display-buffer buf))))
```
