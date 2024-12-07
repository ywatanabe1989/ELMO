<!-- ---
!-- title: ./Semacs/docs/using_a_emacs_server_with_multiple_users.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:33:48
!-- --- -->

# Create shared group
groupadd emacs-users
usermod -a -G emacs-users user1
usermod -a -G emacs-users user2

# Set socket directory permissions
chmod 775 /path/to/socket/dir
chgrp emacs-users /path/to/socket/dir
