<!-- ---
!-- title: 2024-12-31 02:16:27
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/docs/dev/README_old.md
!-- --- -->

``` elisp
(llemacs-run "make a simple plot")
(llemacs-run "make a simple gif")
(llemacs-run "make a simple audio")
```

Context
(wsl) llemacs $ tree workspace/projects/000-PROJECTNAME/
workspace/projects/000-PROJECTNAME/
├── data
├── docs
│   ├── project-dynamic.json
│   └── project-static.json
├── forum.json
├── README.md
├── requirements.txt
├── results
└── scripts

5 directories, 5 files
(wsl) llemacs $ 

cat "$llemacs_home/.emacs.d/init.el"

export LLEMACS_BIND="$(pwd)/workspace:/workspace"
main.sh

main.sh -m shell
/opt/llemacs/apptainer/build/startup/start_emacs.sh

/workspace/llemacss/llemacs-000/.emacs.d/init.el