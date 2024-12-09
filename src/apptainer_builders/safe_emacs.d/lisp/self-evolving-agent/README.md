<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-04 17:24:33
!-- --- -->


# Self-Evolving Agent (SEA) for Emacs

Running Emacs by an agent, just like as a human engineer.

NOW, THIS REPOSITORY IS UNDER DEVELOPMENT.

## Installation

1. Clone repository:
```bash
git clone https://github.com/user/self-evolving-agent.git ~/.emacs.d/lisp/self-evolving-agent
```

2. Add to your Emacs config:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/self-evolving-agent/src")
(require 'sea)
```

3. Call the installation function
```elisp
M-x sea-install
```

## Usage

``` elisp
M-x sea-self-evolve
```

## Working with shell script

``` bash
./src/sea_server.sh [start|stop|restart|status|execute]
./src/sea_server.sh start &
./src/sea_server.sh execute '(message "hello")'
./src/sea_server.sh execute '(message "hihi")'
./src/sea_server.sh execute '(progn (with-current-buffer (get-buffer-create "*test*") (insert "hello")) (switch-to-buffer "*test*"))'
```

## Calling from Elisp

``` elisp
(defun sea-escape-elisp-code (code)
  "Prepare elisp CODE for sea-exec-elisp-code by adding proper escaping."
  (format "'%s'" (prin1-to-string code)))

(sea-exec-elisp-code (sea-escape-elisp-code '(message "hihihi")))

(sea-exec-elisp-code "'\(message \"Hello SEA!!!\"\)'")

(sea-exec-elisp-code
 "'\(progn
    (find-file \"/tmp/test.txt\")
    (insert \"This is a test script.\")
  \)'")
  
(sea-exec-elisp-code 
  "'\(message \"%s\" \(\+ 2 3\)\)'")
```

## Converts Natural Language into elisp code

``` elisp
(sea--prompt-to-elisp "hello world")
```

## Run Natural Language Task on Emacs run by SEA

``` elisp
ss rsync-emacs 
sudo /home/ywatanabe/.bin/safe_rsync /home/emacs-agent/.emacs.d/ /home/sea/.emacs.d/ -- --chown=sea:sea
sudo /home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent/ /home/sea/.emacs.d/lisp/self-evolving-agent/ -- --chown=sea:sea

sudo ls /home/emacs-agent/.emacs.d
sudo ls /home/sea/.emacs.d
sudo chown sea:sea -R /home/sea/.emacs.d


(sea-kill-server)
./src/sea_server.sh start &
(sea-init-or-connect)
(sea-run "make a new buffer and say hi")

(sea-backup-log)
(sea-show-log)







(sea--exec-elisp-code 
(sea--prompt-to-elisp 
  "open a new buffer and write a welcome message, hello world as an ASCII art")
)


;; (sea-exec-elisp-code '(message "hello!!")) # works
;; (sea-exec-elisp-code '(progn (generate-new-buffer *hello*) (switch-to-buffer *hello*) (insert Hi there!))) # not working

(sea-run "make a new buffer and say hi")
(sea-run "open a new buffer and write a welcome message, hello world as an ASCII art")


(sea-run "change the emacs appearance to cool, modern looks")
(sea-run "change the emacs appearance to cool, modern looks; use a cool theme after installation")


;; (sea-exec-elisp-code '(message "hello!!")) # this works
(sea-exec-elisp-code '(progn (message Hello! I'm your self-evolving agent. How can I help you today?)))




```




## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



