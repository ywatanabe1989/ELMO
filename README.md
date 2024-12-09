<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
!-- --- -->


# Semacs:— Self-Evolving AI agent on Emacs

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
Semacs is written in Elisp, run on Emacs, powered by LLMs, and containerized by Apptainer. The choice of Emacs is well-founded because of its:
- Full control via CUI — seamless connection between users and agents
- Built-in GUI functionality
- Mature interfaces (programming execution, text editing, file managing, image rendering, web browsing and more)
- Extensive ecosystems
- Optimized key bindings

Here, we revive Emacs — born in MIT's AI Lab in the 1970s — as an ideal platform for AI agents.


NOW, THIS REPOSITORY IS UNDER DEVELOPMENT
==========================================

## Installation

```bash
git clone https://github.com/ywatanabe1989/seamacs.git ~/.emacs.d/lisp/seamacs
git clone https://github.com/ywatanabe1989/apptainer-utils.git ~/.bash.d/apptainer-utils
```

## Apptainer
Semacs uses Apptainer for permission control

``` bash
apptainer_build_def2sand ./.apptainer/semacs/semacs.def
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp
