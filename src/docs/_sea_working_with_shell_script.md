<!-- ---
!-- title: ./self-evolving-agent/src/docs/working_with_shell_script.md
!-- author: ywatanabe
!-- date: 2024-12-05 19:21:41
!-- --- -->


# Working with shell script

``` bash
./src/sea_server.sh [start|stop|restart|status|execute]
./src/sea_server.sh start &
./src/sea_server.sh execute '(message "hello")'
./src/sea_server.sh execute '(progn (with-current-buffer (get-buffer-create "*test*") (insert "hello")) (switch-to-buffer "*test*"))'
```
