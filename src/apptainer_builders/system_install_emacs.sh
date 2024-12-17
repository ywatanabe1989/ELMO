#!/bin/bash
# Time-stamp: "2024-12-17 15:09:38 (ywatanabe)"
# File: ./Ninja/src/apptainer_builders/install_emacs.sh

# Check if running as root
if [ "$(id -u)" != "0" ]; then
   echo "This script ($0) must be run as root" >&2
   exit 1
fi

source "$(dirname $0)"/ENVS.sh.src

apt install -y \
        emacs \
        emacs-common \
        emacs-bin-common \
        >/dev/null

# Install Nerd Fonts Symbols
cd /tmp
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip
unzip NerdFontsSymbolsOnly.zip -d NerdFontsSymbolsOnly
cd NerdFontsSymbolsOnly
mkdir -p /root/.local/share/fonts
cp *.ttf /root/.local/share/fonts/
fc-cache -f -v

# Install all-the-icons fonts (non-interactive)
emacs --batch --eval "(progn (require 'all-the-icons) (all-the-icons-install-fonts t))"

# Cleanup
cd /tmp
rm -rf NerdFontsSymbolsOnly*

# EOF
