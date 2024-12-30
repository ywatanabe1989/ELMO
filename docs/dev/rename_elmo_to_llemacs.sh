
# First, rename directory contents recursively
find /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs -type f -exec sed -i 's/llemacs/llemacs/g' {} +

# Then rename the directory
mv /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs
