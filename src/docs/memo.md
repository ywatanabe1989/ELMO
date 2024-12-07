<!-- ---
!-- title: ./self-evolving-agent/src/docs/memo.md
!-- author: ywatanabe
!-- date: 2024-12-05 20:52:14
!-- --- -->


ss rsync-emacs 
sudo /home/ywatanabe/.bin/safe_rsync /home/emacs-agent/.emacs.d/ /home/sea/.emacs.d/ -- --chown=sea:sea
sudo /home/ywatanabe/.bin/safe_rsync /home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent/ /home/sea/.emacs.d/lisp/self-evolving-agent/ -- --chown=sea:sea

sudo cp /home/ywatanabe/.emacs.d/inits/300-tools/999-sea.el /home/sea/.emacs.d/inits/300-tools/999-sea.el
sudo chown sea:sea /home/sea/.emacs.d/inits/300-tools/999-sea.el

sudo chmod 770 /home/ywatanabe/.sea/logs/history.log

sudo ls /home/emacs-agent/.emacs.d
sudo ls /home/sea/.emacs.d
sudo chown sea:sea -R /home/sea/.emacs.d


(sea-kill-server)
./src/sea_server.sh start
(sea-restart-server)
(sea-init-or-connect)
(sea-run "make a new buffer and say hi")


(sea-backup-log)
(sea-show-log)

(sea-run "open a new buffer and write a welcome message, hello world as an ASCII art")
(sea-run "open a new buffer and write a welcome message, hello world to SEA as an ASCII art, with cool and modern vibe, in the markdown mode or org mode for better visualization")

(sea-run "update this ascii art for better looks

  ▄▄▄▄▄▄▄  ▄▄▄▄▄▄  ▄▄▄▄▄▄▄
  █ ▄▄▄▄█  █ ▄▄▄█  █ ▄▄▄ █
  █ █▄▄▄▄  █ █▄▄▄  █ █ █ █
  █▄▄▄▄ █  █ ▄▄▄█  █ █▄█ █
  ▄▄▄▄█ █  █ █▄▄▄  █ █ █ █
  █▄▄▄▄▄█  █▄▄▄▄█  █▄█ █▄█

  ========================
  Self-Evolving Agent v1.0
  ========================

")

(sea-run "write a logo of SEA for your branding")

sudo chown sea:sea -R /home/sea/.emacs.d/lisp/self-evolving-agent/src/
sudo chown sea:sea -R /home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent/src/


;; (sea-exec-elisp-code '(message "hello!!")) # works
;; (sea-exec-elisp-code '(progn (generate-new-buffer *hello*) (switch-to-buffer *hello*) (insert Hi there!))) # not working

(sea-run "make a new buffer and say hi")
(sea-run "open a new buffer and write a welcome message, hello world as an ASCII art")


(sea-run "change the emacs appearance to cool, modern looks")
(sea-run "change the emacs appearance to cool, modern looks; use a cool theme after installation")



(sea-run "
write a python code to calculate DMD
plot an example figure and save it as jpeg
show the image on a right buffer
")

;; (sea-exec-elisp-code '(message "hello!!")) # this works
(sea-exec-elisp-code '(progn (message Hello! I'm your self-evolving agent. How can I help you today?)))






local$ ssh -R12345:localhost:12345 remote
remote$ export EDITOR="emacsclient \
        --server-file=server \
        --tramp=/ssh:remote:"
remote$ $EDITOR /tmp/foo.txt #Should open in local emacs.
