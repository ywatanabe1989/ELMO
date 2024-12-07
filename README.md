<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:10:44
!-- --- -->


# SEMACS: Self-Evolving AI agent on Emacs

``` plaintext
███████╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
██╔════╝██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
███████╗█████╗  ██╔████╔██║███████║██║     ███████╗
╚════██║██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
███████║███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
===================================================
Version 0.1.0
```

## Introduction
Semacs, written in Elisp, offers unique advantages:

- Purely text-based system
- Native Emacs accessibility, just like hackers
- Direct integration/customization of Emacs ecosystem
- Seamless user interaction
- Central hub for computing operations
  - Elisp commands
  - Text processing
  - Shell operations
  - Development tools
  - File management
  - Web access
  - Email services
  - ... and more
- Secure permission control using Apptainer

Here, we revive Emacs - born in MIT's AI Lab in the 1970s - as a modern platform for AI agents.


==========================================
NOW, THIS REPOSITORY IS UNDER DEVELOPMENT
==========================================

## Installation

```bash
git clone https://github.com/ywatanabe1989/seamacs.git ~/.emacs.d/lisp/seamacs
```

## Configuration
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/self-evolving-agent/src")
(require 'sea)
(semacs-install)
```

## Launch an Emacs Window by the SEA user
``` bash
sudo echo aaa && sudo ./src/sea_server.sh start
sudo ./src/sea_server.sh kill
sudo ./src/sea_server.sh init

(semacs-init-server)
```

## Reload source of self-evolving-agent on the SEA emacs
``` bash
<!-- (message (format "%s" default-directory)) -> /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/ -->
sudo chmod 774 -R /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/
sudo chown ywatanabe:sea -R /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/
sudo chmod 770 /home/sea/.emacs.d/server/server
(semacs-exec-elisp-code '(load-file "/home/sea/.emacs.d/init.el"))
```

## Edit on the SEA EMACS

``` bash
sudo usermod -a -G sea ywatanabe
```

## Working from your own Emacs session

``` elisp
(semacs--sudo-get-password)
(semacs-run "show welcome message")
(semacs-run "open google")
(semacs-run "write python code which calculates DMD from EEG demo signal and visualize results.")
(semacs-run "using the internet, perform literature review regarding epilepsy seizure prediction from bio signals")


sudo chmod 775 /home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent/
```






## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



