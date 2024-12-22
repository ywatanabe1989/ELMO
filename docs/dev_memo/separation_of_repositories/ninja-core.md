<!-- ---
!-- title: 2024-12-22 22:39:01
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/Ninja/docs/dev_memo/separation_of_repositories/ninja-core.md
!-- --- -->

mkdir ~/proj/ninja
cd ~/proj/ninja
mkdir ninja-core ninja-builders ninja-workspace

git clone https://github.com/yourusername/ninja-core.git
git clone https://github.com/yourusername/ninja-builders.git
git clone https://github.com/yourusername/ninja-workspace.git


ninja-core is just a elisp package?

ninja-core/
├── src/
│   ├── elisp/
│   │   ├── ninja.el         # Main package file
│   │   ├── ninja-config.el
│   │   ├── ninja-exec.el
│   │   └── ...
│   └── prompts/
│       ├── lang2elisp.md
│       └── ...
├── tests/
├── docs/
├── LICENSE
└── README.md

ninja-builders/
├── apptainer/
│   ├── builders/
│   ├── configs/
│   └── scripts/
├── docker/
├── scripts/
│   ├── install/
│   └── setup/
├── LICENSE
└── README.md

ninja-workspace/
├── templates/
│   ├── projects/
│   └── protocols/
├── examples/
├── resources/
├── docs/
├── LICENSE
└── README.md