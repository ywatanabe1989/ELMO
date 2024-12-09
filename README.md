<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
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
Semacs, written in Elisp within Apptainer container, offers unique advantages:

- Full CUI environment with GUI support (= backend + frontend)
- Seamless user interaction
- Emacs ecosystem and customization:
  - Any computer language, Text editing, Filer, Image viewer, Web viewer, Email server, and more
- Permission control via Apptainer

Here, we revive Emacs - born in MIT's AI Lab in the 1970s - as a modern platform for AI agents.


NOW, THIS REPOSITORY IS UNDER DEVELOPMENT
==========================================

## Installation

```bash
git clone https://github.com/ywatanabe1989/seamacs.git ~/.emacs.d/lisp/seamacs
git clone https://github.com/ywatanabe1989/apptainer-utils.git ~/.bash.d/apptainer-utils
```

## Apptainer
abuild_def2sand ./.apptainer/semacs/semacs.def

## Contact
ywatanabe@alumni.u-tokyo.ac.jp
