<!-- ---
!-- title: ./LLEMACS/docs/dev_memo/crdt.md
!-- author: ywatanabe
!-- date: 2024-12-18 09:15:18
!-- --- -->


For using CRDT in Apptainer containers:

1. Install in each container:
```elisp
(package-install 'crdt)
```

2. Server setup (one container):
```elisp
M-x crdt-share-buffer
; Enter session name
; Enter port (default 6530)
; Set password if needed
```

3. Client setup (other containers):
```elisp
M-x crdt-connect
; Enter server address (container IP/hostname)
; Enter port
; Enter display name
```

Key points for containers:
- Ensure network connectivity between containers
- Port must be accessible (may need `--net` in Apptainer)
- All containers need same CRDT version
- Consider using `--bind` for persistent storage if needed

Common workflow:
1. Start server in one container
2. Share URL/credentials with other users
3. Others connect from their containers
4. Use `crdt-list-buffers` to manage shared files
