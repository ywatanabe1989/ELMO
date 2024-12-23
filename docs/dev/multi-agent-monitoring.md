<!-- ---
!-- title: ./ELMO/docs/dev_memo/monitoring.md
!-- author: ywatanabe
!-- date: 2024-12-16 19:46:23
!-- --- -->


# Monitoring
## Monitoring buffer

``` elisp
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
```


## Agents' cursors
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
