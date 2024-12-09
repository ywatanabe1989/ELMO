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
Semacs is an LLM agents system written in Elisp, and runs on Emacs within Apptainer. The choice of Emacs is well-founded because of its:
- Full control via CUI — seamless connection between users and agents
- Built-in visualization capabilities (e.g., image rendering, web browsing)
- Mature interfaces (e.g., programming execution, text editing, file managing)
- Extensive ecosystems
- Reproducibility (i.e., Semacs is written in Elisp and can write Elisp)

Here, we revive Emacs — born in MIT's AI Lab in the 1970s — as an ideal platform for AI agents.


NOW, THIS REPOSITORY IS UNDER DEVELOPMENT
==========================================

## Installation

```bash
git clone https://github.com/ywatanabe1989/seamacs.git ~/.emacs.d/lisp/seamacs
git clone https://github.com/ywatanabe1989/apptainer-utils.git ~/.bash.d/apptainer-utils
```

#### Apptainer

``` bash
apptainer_build_def2sand ./.apptainer/semacs/semacs.def
apptainer run ./.apptainer/semacs/semacs.sandbox
```

## Contact
ywatanabe@alumni.u-tokyo.ac.jp
