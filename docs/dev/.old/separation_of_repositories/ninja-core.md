<!-- ---
!-- title: 2024-12-22 15:57:27
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/ELMO/docs/dev_memo/separation_of_repositories/llemacs-core.md
!-- --- -->

mkdir ~/proj/llemacs
cd ~/proj/llemacs
git clone https://github.com/yourusername/llemacs-core.git
git clone https://github.com/yourusername/llemacs-builders.git
git clone https://github.com/yourusername/llemacs-workspace.git

llemacs-core/
├── src/
│   ├── elisp/
│   │   ├── llemacs.el         # Main package file
│   │   ├── llemacs-config.el
│   │   ├── llemacs-exec.el
│   │   └── ...
│   └── prompts/
│       ├── lang2elisp.md
│       └── ...
├── tests/
├── docs/
├── LICENSE
└── README.md

llemacs-builders/
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

llemacs-workspace/
├── templates/
│   ├── projects/
│   └── protocols/
├── examples/
├── resources/
├── docs/
├── LICENSE
└── README.md