<!-- ---
!-- title: 2024-12-22 15:57:27
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/ELMO/docs/dev_memo/separation_of_repositories/elmo-core.md
!-- --- -->

mkdir ~/proj/elmo
cd ~/proj/elmo
git clone https://github.com/yourusername/elmo-core.git
git clone https://github.com/yourusername/elmo-builders.git
git clone https://github.com/yourusername/elmo-workspace.git

elmo-core/
├── src/
│   ├── elisp/
│   │   ├── elmo.el         # Main package file
│   │   ├── elmo-config.el
│   │   ├── elmo-exec.el
│   │   └── ...
│   └── prompts/
│       ├── lang2elisp.md
│       └── ...
├── tests/
├── docs/
├── LICENSE
└── README.md

elmo-builders/
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

elmo-workspace/
├── templates/
│   ├── projects/
│   └── protocols/
├── examples/
├── resources/
├── docs/
├── LICENSE
└── README.md