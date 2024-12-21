## ProxyCommand in SSH config:
- Specifies a command to use for connecting to the server
- In this case, it's used for SSH tunneling through a bastion host
- `ssh -CW %h:%p bastion-jp` means:
  - Connect to bastion-jp first
  - Then use this connection to reach the final destination
  - `-C`: Enable compression
  - `-W`: Request forwarding of stdin/stdout to host:port
  - `%h:%p`: Placeholder for destination host and port
