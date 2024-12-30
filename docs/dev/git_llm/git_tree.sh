# git-tree() {
#     git ls-tree -r --name-only HEAD | tree --fromfile
# }

git_tree() {
    tree --gitignore
    }


