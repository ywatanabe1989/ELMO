<!-- ---
!-- title: ./Semacs/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:12:12
!-- --- -->


# SEMACS — Self-Evolving AI agent on Emacs

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
Semacs is written in Elisp, run on Emacs, powered by LLMs, and containerized by Apptainer. The choice of Emacs is well-founded bacause of:
- Full control via CUI: seamless connection among users and LLMs
- Built-in GUI functionality
- Extensive ecosystems
- Mature programming interfaces

Here, we revive Emacs — born in MIT's AI Lab in the 1970s — as an ideal platform for AI agents.


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



<!-- - Full CUI environment with GUI support (= backend + frontend)
 !-- - Seamless user interaction
 !-- - Emacs extensive ecosystem:
 !--   - Built-in commands
 !--   - Extensive customization with the users
 !--   - Any computer language, Text editing, Filer, Image viewer, Web viewer, Email server, and more
 !-- - Permission control via Apptainer -->
